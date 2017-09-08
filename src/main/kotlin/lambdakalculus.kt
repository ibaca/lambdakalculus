import java.util.concurrent.atomic.AtomicInteger

fun main(args: Array<String>) {
    val id = Lambda(Var("x"), Var("x"))
    assert(LambdaCalculusParser(id.prettyPrint()).equals(id))

    val one = Lambda(Var("s"), Lambda(Var("z"), Apply(Var("s"), Var("z"))))
    assert(LambdaCalculusParser(one.prettyPrint()).equals(one))

    println("token to string: " + one.prettyPrint())
    println("string to token: " + LambdaCalculusParser("λs.(λz.(!s z))"))
    println("with bindings: " + bind(LambdaCalculusParser("λx.(λx.(!x x))"), Scope.TOP).prettyPrint())
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

fun Expr.prettyPrint() = when (this) {
    is Lambda -> "λ${parentIfNeeded(arg)}.${parentIfNeeded(body)}"
    is Apply -> "!${parentIfNeeded(fn)} ${parentIfNeeded(arg)}"
    is Var -> name + scope.id
}

fun parentIfNeeded(expr: Expr): String = (expr as? Var)?.prettyPrint() ?: "(${expr.prettyPrint()})"

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
    val variable = repeat(string().word()).map("variable") { Var(it.joinToString("")) }
    val application = string().char('!').and(expr).zip({ string().ws().andR(expr) }, { v, w, r -> Success("application", Apply(v, w), r) })
    val lambda = (string().char('λ') and variable).zip({ string().char('.') and expr }, { v, w, r -> Success("lambda", Lambda(v, w), r) })
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
fun <T, V, W> Parser<T, V>.map(msg: String, fn: (V) -> W): Parser<T, W> = compose { it.mapSuccess { (_, value, rest) -> Success(msg, fn(value), rest) } }
fun <T, V, W, R> Parser<T, V>.zip(wFn: (V) -> Parser<T, W>, fn: (V, W, T) -> Result<T, R>): Parser<T, R> = compose {
    it.mapSuccess { v -> wFn(v.value)(v.rest).mapSuccess { (_, value, rest) -> fn(v.value, value, rest) } }
}

infix fun <T, V> Parser<T, V>.or(other: Parser<T, V>): Parser<T, V> = Parser({ this(it).mapFailure { _ -> other(it) } })
infix fun <T, V, W> Parser<T, V>.and(other: Parser<T, W>) = andR(other)
infix fun <T, V, W> Parser<T, V>.andR(other: Parser<T, W>) = zip({ other }) { _, v, r -> Success("andR", v, r) }
infix fun <T, V, W> Parser<T, V>.andL(other: Parser<T, W>) = zip({ other }) { v, _, r -> Success("andL", v, r) }
fun <T, V> Parser<T, V>.wrapError(msg: String) = compose { it.mapFailure { f -> Failure(msg, f, f.rest) } }
fun <T, V> Parser<T, V>.between(start: Parser<T, *>, end: Parser<T, *> = start) = (start andR this andL end)
fun <T, V> Parser<T, V>.list() = compose { it.mapSuccess { (_, value, rest) -> Success("list", listOf(value), rest) } }

fun <T, V> succeed(msg: String, value: V): Parser<T, V> = Parser({ Success(msg, value, it) })
fun <T, V> empty(): Parser<T, List<V>> = succeed("empty", emptyList())
fun <T, V> repeatOrEmpty(parser: Parser<T, V>): Parser<T, List<V>> = repeat(parser) or empty()
fun <T, V> repeat(parser: Parser<T, V>) = parser.zip({ _ -> repeatOrEmpty(parser) }, { v, l, r -> Success("repeat", arrayListOf(v) + l, r) })

fun <T> Parser<T, Char>.char(ch: Char) = compose { it.filter({ it == ch }) }
fun <T> Parser<T, Char>.char(regex: Regex) = compose { it.filter({ regex.matches(it.toString()) }) }
fun <T> Parser<T, Char>.char(test: (Char) -> Boolean) = compose { it.filter(test) }
fun <T> Parser<T, Char>.ws() = repeat(char(Regex("""\s""")))
fun <T> Parser<T, Char>.word() = char(Regex("""\w"""))
fun <T> Parser<T, Char>.token() = repeat(word()).between(ws())
fun <T> Parser<T, Char>.prefix(prefix: Char, parser: Parser<T, List<Char>>) = concat(char(prefix), parser) or parser

fun <T> concat(p1: Parser<T, Char>, p2: Parser<T, List<Char>>): Parser<T, List<Char>> {
    return p1.zip({ p2 }, { v: Char, l: List<Char>, r -> Success("concat", arrayListOf(v) + l, r) })
}

fun <T> concat(vararg charParsers: Parser<T, Char>): Parser<T, List<Char>> {
    return charParsers.fold(empty()) { acc, n -> acc.zip({ n }, { xs, x, r -> Success("concat", xs + x, r) }) }
}

fun string() = Parser<String, Char> {
    when (it.length) {
        0 -> Failure("EOF", null, "")
        1 -> Success("DONE", it[0], "")
        else -> Success("NEXT", it[0], it.substring(1))
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

data class Success<T, V>(val msg: String, val value: V, val rest: T) : Result<T, V>()
data class Failure<T, V>(val msg: String, val child: Failure<T, *>?, val rest: T) : Result<T, V>()

@Suppress("UNCHECKED_CAST")
fun <T, V> Failure<T, *>.cast(): Result<T, V> = this as Result<T, V>


