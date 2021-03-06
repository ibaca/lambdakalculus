import java.util.concurrent.atomic.AtomicInteger

fun main(args: Array<String>) {
    fun parse(str: String): Expr = LambdaCalculusParser.invoke(str)
    val id = Lambda(Var("x"), Var("x"))
    assert(parse(id.pretty()).equals(id))

    val one = Lambda(Var("s"), Lambda(Var("z"), Apply(Var("s"), Var("z"))))
    assert(parse(one.pretty()).equals(one))

    println("token to string: " + one.pretty())
    println("string to token: " + parse("λs.(λz.(!s z))"))
    // println("with bindings: " + bind(parse("λx.(λx.(!x x))"), Scope.TOP).pretty())
    println("after evaluation: " + eval(bind(parse("!(λx.!(λy.(λx.y)) x) (λx.x)"), Scope.TOP), true).pretty())

    // Ups… this should be left-associative
    println("!a !b c: " + parse("!a !b c"))
    println("!a (!b c): " + parse("!a (!b c)"))
    println("expected to be false: " + parse("!a !b c").equals(parse("!a (!b c)")))
    println("expected to be true: " + parse("!a !b c").equals(parse("!(!a b) c")))
}

sealed class Expr
data class Lambda(val arg: Var, val body: Expr) : Expr()
data class Var(val name: String, val scope: Scope = Scope.TOP) : Expr()
data class Apply(val fn: Expr, val arg: Expr) : Expr()

data class Scope(val parent: Scope?, val boundNames: Set<String>) {
    val id = next.getAndIncrement()

    fun closest(name: String): Scope? = when {
        boundNames.contains(name) -> this
        else -> parent?.closest(name)
    }

    companion object {
        val next = AtomicInteger(0)
        val TOP = Scope(null, emptySet())
    }
}

fun bind(term: Expr, parent: Scope): Expr = when (term) {
    is Lambda -> {
        val scope = Scope(parent, setOf(term.arg.name))
        Lambda(Var(term.arg.name, scope), bind(term.body, scope))
    }
    is Var -> {
        Var(term.name, parent.closest(term.name) ?: throw Throwable("undefined variable ${term.name}"))
    }
    is Apply -> {
        Apply(bind(term.fn, parent), bind(term.arg, parent))
    }
}

fun eval(term: Expr, debug: Boolean = false): Expr {
    fun Expr.isValue(): Boolean = when (this) {
        is Lambda -> true
        is Var -> true
        is Apply -> false
    }

    fun evalStep(term: Expr): Expr? = when {
        term is Apply && term.fn is Lambda && term.arg.isValue() -> substitution(term.fn.arg, term.arg)(term.fn.body)
        term is Apply && term.fn.isValue() -> evalStep(term.arg)?.let { Apply(term.fn, it) }
        term is Apply -> evalStep(term.fn)?.let { Apply(it, term.arg) }
        else -> null
    }

    tailrec fun apply(term: Expr): Expr {
        val evalStep = evalStep(term)
        return if (evalStep == null) term else {
            if (debug) println("step: ${term.pretty()}  → ${evalStep.pretty()}")
            apply(evalStep)
        }
    }
    return apply(term)
}

fun substitution(argV: Var, replacement: Expr): (term: Expr) -> Expr {
    fun apply(it: Expr): Expr = when {
        it is Var && it.name == argV.name && it.scope == argV.scope -> bind(replacement, argV.scope.parent!!)
        it is Var -> it
        it is Apply -> Apply(apply(it.fn), apply(it.arg))
        it is Lambda -> Lambda(it.arg, apply(it.body))
        else -> throw NoWhenBranchMatchedException("non substitutable")
    }
    return { apply(it) }
}

fun Expr.pretty() = when (this) {
    is Lambda -> "λ${parentIfNeeded(arg)}.${parentIfNeeded(body)}"
    is Apply -> "!${parentIfNeeded(fn)} ${parentIfNeeded(arg)}"
    is Var -> name + scope.id
}

fun parentIfNeeded(expr: Expr): String = (expr as? Var)?.pretty() ?: "(${expr.pretty()})"

/**
 * ```
 * def expr        = lambda | application | variable | parens
 * def lambda      = "λ" ~ variable ~ "." ~ expr
 * def application = "!" ~ expr ~ expr
 * def parens      = "(" ~ expr ~ ")"
 * def variable    = ident
 * ```
 * NOTE: added "!" to 'application' to avoid left recursion!
 */
object LambdaCalculusParser {
    val expr = Parser<String, Expr>()
    val variable = repeat(string().word()).map { Var(it.joinToString("")) }
    val application = string().char('!').and(expr).zip({ string().ws().andR(expr) }, { v, w, r -> Success(Apply(v, w), r) })
    val lambda = (string().char('λ') and variable).zip({ string().char('.') and expr }, { v, w, r -> Success(Lambda(v, w), r) })
    val parentheses = expr.between(string().char('('), string().char(')'))

    init {
        expr.set(parentheses or lambda.cast() or application.cast() or variable.cast())
    }

    operator fun invoke(input: String): Expr = expr(input).get()
}

/**
 * Parser Combinator library for Kotlin - https://github.com/absurdhero/parsekt
 * Monadic Parser Combinators - http://blogs.msdn.com/b/lukeh/archive/2007/08/19/monadic-parser-combinators-using-c-3-0.aspx
 */
class Parser<T, V>() {
    //@formatter:off late-init and setter to allow mutually recursive parsers
    private lateinit var fn: (T) -> Result<T, V>
    constructor(fn: (T) -> Result<T, V>) : this() { set(fn) }
    fun set(fn: Parser<T, V>) { set(fn.fn) }
    fun set(fn: (T) -> Result<T, V>) { this.fn = fn }
    //@formatter:on

    operator fun invoke(input: T): Result<T, V> = fn(input)

    @Suppress("UNCHECKED_CAST")
    fun <W> cast() = this as Parser<T, W>
}

fun <T, V, W> Parser<T, V>.compose(fn: (Result<T, V>) -> (Result<T, W>)): Parser<T, W> = Parser { fn(this(it)) }
fun <T, V, W> Parser<T, V>.map(fn: (V) -> W): Parser<T, W> = compose { it.mapSuccess { (value, rest) -> Success(fn(value), rest) } }
fun <T, V, W, R> Parser<T, V>.zip(wFn: (V) -> Parser<T, W>, fn: (V, W, T) -> Result<T, R>): Parser<T, R> = compose {
    it.mapSuccess { v -> wFn(v.value)(v.rest).mapSuccess { (value, rest) -> fn(v.value, value, rest) } }
}

infix fun <T, V> Parser<T, V>.or(other: Parser<T, V>): Parser<T, V> = Parser { this(it).mapFailure { _ -> other(it) } }
infix fun <T, V, W> Parser<T, V>.and(other: Parser<T, W>) = andR(other)
infix fun <T, V, W> Parser<T, V>.andR(other: Parser<T, W>) = zip({ other }) { _, v, r -> Success(v, r) }
infix fun <T, V, W> Parser<T, V>.andL(other: Parser<T, W>) = zip({ other }) { v, _, r -> Success(v, r) }
fun <T, V> Parser<T, V>.wrapError(msg: String) = compose { it.mapFailure { f -> Failure(msg, f, f.rest) } }
fun <T, V> Parser<T, V>.between(start: Parser<T, *>, end: Parser<T, *> = start) = (start andR this andL end)
fun <T, V> Parser<T, V>.list() = compose { it.mapSuccess { (value, rest) -> Success(listOf(value), rest) } }

fun <T, V> succeed(value: V): Parser<T, V> = Parser { Success(value, it) }
fun <T, V> empty(): Parser<T, List<V>> = succeed(emptyList())
fun <T, V> repeatOrEmpty(parser: Parser<T, V>): Parser<T, List<V>> = repeat(parser) or empty()
fun <T, V> repeat(parser: Parser<T, V>) = parser.zip({ _ -> repeatOrEmpty(parser) }, { v, l, r -> Success(arrayListOf(v) + l, r) })

fun <T> Parser<T, Char>.char(ch: Char) = compose { it.filter { c -> c == ch } }
fun <T> Parser<T, Char>.char(regex: Regex) = compose { it.filter { c -> regex.matches(c.toString()) } }
fun <T> Parser<T, Char>.char(test: (Char) -> Boolean) = compose { it.filter(test) }
fun <T> Parser<T, Char>.ws() = repeat(char(Regex("""\s""")))
fun <T> Parser<T, Char>.word() = char(Regex("""\w"""))
fun <T> Parser<T, Char>.token() = repeat(word()).between(ws())
fun <T> Parser<T, Char>.prefix(prefix: Char, parser: Parser<T, List<Char>>) = concat(char(prefix), parser) or parser

fun <T> concat(p1: Parser<T, Char>, p2: Parser<T, List<Char>>): Parser<T, List<Char>> =
        p1.zip({ p2 }, { v: Char, l: List<Char>, r -> Success(arrayListOf(v) + l, r) })

fun <T> concat(vararg charParsers: Parser<T, Char>): Parser<T, List<Char>> =
        charParsers.fold(empty()) { acc, n -> acc.zip({ n }, { xs, x, r -> Success(xs + x, r) }) }

fun string() = Parser<String, Char> {
    when (it.length) {
        0 -> Failure("EOF", null, "")
        1 -> Success(it[0], "")
        else -> Success(it[0], it.substring(1))
    }
}

sealed class Result<T, V> {

    fun <W> mapSuccess(fn: (Success<T, V>) -> Result<T, W>): Result<T, W> = when (this) {
        is Success -> fn(this)
        is Failure -> this.cast()
    }

    fun mapFailure(fn: (Failure<T, V>) -> Result<T, V>): Result<T, V> = when (this) {
        is Success -> this
        is Failure -> fn(this)
    }

    fun filter(fn: (V) -> Boolean): Result<T, V> = when (this) {
        is Success -> if (fn(value)) this else Failure("filter", null, rest)
        is Failure -> this.cast()
    }

    fun get(): V = when (this) {
        is Success -> value
        is Failure -> throw RuntimeException("parse error: $this")
    }
}

data class Success<T, V>(val value: V, val rest: T) : Result<T, V>()
data class Failure<T, V>(val msg: String, val child: Failure<T, *>?, val rest: T) : Result<T, V>()

@Suppress("UNCHECKED_CAST")
fun <T, V> Failure<T, *>.cast(): Result<T, V> = this as Result<T, V>


