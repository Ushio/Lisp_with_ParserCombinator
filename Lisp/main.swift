//
//  main.swift
//  Lisp
//
//  Created by Ushio on 2015/03/31.
//  Copyright (c) 2015年 Ushio. All rights reserved.
//

import Foundation

import Foundation

enum List<T> {
    case Cons(T, () -> List<T>)
    case Nil
}

func cons<T>(value: T, list: List<T>) -> List<T> {
    return List.Cons(value) { list }
}

func one<T>(value: T) -> List<T> {
    return cons(value, .Nil)
}
func none<T>() -> List<T> {
    return .Nil
}
func lazyCons<T>(value: T, f: () -> List<T>) -> List<T> {
    return List.Cons(value, f)
}

extension List {
    var car: T? {
        switch self {
        case let .Cons(car, _):
            return car
        case .Nil:
            return nil
        }
    }
    var cdr: List<T> {
        switch self {
        case let .Cons(_, cdr):
            return cdr()
        case .Nil:
            return .Nil
        }
    }
}

struct ListGenerator<T> : GeneratorType {
    typealias Element = T
    
    init(_ list: List<T>) {
        _list = list
    }
    
    mutating func next() -> Element? {
        let car = _list.car
        _list = _list.cdr
        return car
    }
    var _list: List<T>
}

extension List : SequenceType {
    func generate() -> ListGenerator<T> {
        return ListGenerator(self)
    }
}
extension List {
    var toArray: [T] {
        var r = [T]()
        for n in self {
            r += [n]
        }
        return r
    }
}
extension Array {
    var toList: List<T> {
        var list = List<T>.Nil
        for value in self.reverse() {
            list = cons(value, list)
        }
        return list
    }
}

extension List {
    func take(n: Int) -> List<T> {
        if n > 0 {
            if let v = self.car {
                return lazyCons(v) { self.cdr.take(n - 1) }
            }
        }
        return .Nil
    }
    
    func drop(n: Int) -> List<T> {
        if n > 0 {
            if let v = self.car {
                return self.cdr.drop(n - 1)
            }
        }
        return self
    }
    
    func map<U>(f: T -> U) -> List<U> {
        if let car = self.car {
            return lazyCons(f(car)) { self.cdr.map(f) }
        }
        return .Nil
    }
    
    func filter(f: T -> Bool) -> List<T> {
        if let car = self.car {
            if f(car) {
                return lazyCons(car) { self.cdr.filter(f) }
            } else {
                return self.cdr.filter(f)
            }
        }
        return .Nil
    }
    func reduce<R>(var initial: R, combine: (R, T) -> R) -> R {
        for value in self {
            initial = combine(initial, value)
        }
        return initial
    }
    
    var reverse: List<T> {
        if let car = self.car {
            return self.cdr.reverse + one(car)
        }
        return none()
    }
    var length: Int {
        if let _ = self.car {
            return 1 + self.cdr.length
        }
        return 0
    }
}

func +<T>(a: List<T>, b: List<T>) -> List<T> {
    if let v = a.car {
        return lazyCons(v) { a.cdr + b }
    }
    return b
}

func zip<T, U>(a: List<T>, b: List<U>) -> List<(T, U)> {
    if let va = a.car, vb = b.car {
        return lazyCons((va, vb)) { zip(a.cdr, b.cdr) }
    }
    return .Nil
}
func flatten<T>(list: List<List<T>>) -> List<T> {
    if let out_car: List<T> = list.car {
        if let in_car: T = out_car.car {
            return lazyCons(in_car) { flatten(cons(out_car.cdr, list.cdr)) }
        } else {
            return flatten(list.cdr)
        }
    }
    return .Nil
}
extension List {
    func flatMap<U>(f: T -> List<U>) -> List<U> {
        return flatten(self.map(f))
    }
}

func iterate<T>(v: T, f: T -> T) -> List<T> {
    return lazyCons(v) { iterate(f(v), f) }
}

func pair<T, U>(lhs: List<T>, rhs: List<U>) -> List<(T, U)> {
    return lhs.flatMap { lhsValue in
        return rhs.map { rhsValue in
            (lhsValue, rhsValue)
        }
    }
}

func repeat<T>(value: T) -> List<T> {
    return lazyCons(value) { repeat(value) }
}

extension List {
    var cycle: List<T> {
        return repeat(1).flatMap { _ in
            return self
        }
    }
    func cycle(n: Int) -> List<T> {
        return repeat(1).take(n).flatMap { _ in
            return self
        }
    }
}


// Lisp
enum Atom {
    case Symbol(String)
    case Number(Double)
}
extension Atom {
    var symbol: String? {
        switch self {
        case let .Symbol(symbol):
            return symbol
        default:
            return nil
        }
    }
    var number: Double? {
        switch self {
        case let .Number(number):
            return number
        default:
            return nil
        }
    }
    
    init(_ symbol: String) {
        self = .Symbol(symbol)
    }
    init(_ number: Double) {
        self = .Number(number)
    }
}
extension Atom : Printable {
    var description: String {
        if let symbol = self.symbol {
            return symbol
        } else if let number = self.number {
            return "\(number)"
        }
        return ""
    }
}
enum SExpr {
    case SAtom(Atom)
    case SList(() -> List<SExpr>)
}
extension SExpr {
    var atom: Atom? {
        switch self {
        case let .SAtom(atom):
            return atom
        default:
            return nil
        }
    }
    var list: List<SExpr>? {
        switch self {
        case let .SList(f):
            return f()
        default:
            return nil
        }
    }
    
    init(_ atom: Atom) {
        self = .SAtom(atom)
    }
    init(_ list: List<SExpr>) {
        self = .SList({ list })
    }
}
extension SExpr : Printable {
    var description: String {
        if let atom = self.atom {
            return atom.description
        } else if let list = self.list {
            let strings = list.map { x in x.description }
            if let car = strings.car {
                return "(" + strings.cdr.reduce(car) { r, x in r + " " + x } + ")"
            }
            return ""

        }
        return ""
    }
}

func SExprNil() -> SExpr{
    return SExpr(none())
}

func eval(expr: SExpr) -> SExpr {
    if
        let list = expr.list,
        let car = list.car,
        let symbol = eval(car).atom?.symbol
    {
        let arguments = list.cdr
        return apply(symbol, arguments)
    }
    
    if let atom = expr.atom {
        return expr
    }
    
    return SExprNil()
}

func apply(symbol: String, arguments: List<SExpr>) -> SExpr {
    let number_arguments: List<Double> = arguments.flatMap { x in
        if let number = eval(x).atom?.number {
            return one(number)
        }
        return none()
    }
    let functions: [String : (Double, Double) -> Double] = [
        "+" : (+),
        "-" : (-),
        "*" : (*),
        "/" : (/)
    ]
    if let op = functions[symbol], car = number_arguments.car {
        return SExpr(Atom(number_arguments.cdr.reduce(car, combine: op)))
    }
    return SExprNil()
}

//let s = SExpr([SExpr(Atom("*")), SExpr([SExpr(Atom("+")), SExpr(Atom(3.0)), SExpr(Atom(1.0)), SExpr(Atom(2.0))].toList), SExpr(Atom(2.0))].toList)
//println(s)
//println(eval(s))

// コンビネータ
struct Combinator<Token, Result> {
    typealias Parser = List<Token> -> List<(Result, List<Token>)>
}

// 基本三大要素
func pure<Token, Result>(result: Result) -> Combinator<Token, Result>.Parser {
    return { input in one((result, input)) }
}
func zero<Token, Result>() -> Combinator<Token, Result>.Parser {
    return { input in none() }
}
func consumer<Token>() -> Combinator<Token, Token>.Parser {
    return { input in
        if let car = input.car {
            return one((car, input.cdr))
        }
        return none()
    }
}

infix operator <|> { associativity left precedence 130 }
func <|> <Token, Result>(lhs: Combinator<Token, Result>.Parser, rhs: Combinator<Token, Result>.Parser) -> Combinator<Token, Result>.Parser {
    return { input in lhs(input) + rhs(input) }
}

func bind<Token, T, U>(parser: Combinator<Token, T>.Parser, factory: T -> Combinator<Token, U>.Parser) -> Combinator<Token, U>.Parser {
    return { input in
        let lhsResult = parser(input)
        return lhsResult.flatMap { (result, remainder) in
            let rhsParser = factory(result)
            return rhsParser(remainder)
        }
    }
}

func satisfy<Token>(condition: Token -> Bool) -> Combinator<Token, Token>.Parser {
    return bind(consumer()) { result in
        if condition(result) {
            return pure(result)
        }
        return zero()
    }
}

func token<Token: Equatable>(t: Token) -> Combinator<Token, Token>.Parser {
    return satisfy { $0 == t }
}

func oneOrMore<Token, Result>(parser: Combinator<Token, Result>.Parser, buffer: List<Result>) -> Combinator<Token, List<Result>>.Parser {
    return bind(parser) { result in
        let combine = buffer + one(result)
        return oneOrMore(parser, combine) <|> pure(combine)
    }
}

func oneOrMore<Token, Result>(parser: Combinator<Token, Result>.Parser) -> Combinator<Token, List<Result>>.Parser {
    return oneOrMore(parser, none())
}

func zeroOrMore<Token, Result>(parser: Combinator<Token, Result>.Parser) -> Combinator<Token, List<Result>>.Parser {
    return oneOrMore(parser) <|> pure(none())
}

extension NSCharacterSet {
    var parser: Combinator<Character, Character>.Parser {
        return satisfy { token in
            let unichar = (String(token) as NSString).characterAtIndex(0)
            return self.characterIsMember(unichar)
        }
    }
}

enum LispToken {
    case ParenthesesL
    case ParenthesesR
    case Symbol(String)
    case Number(Double)
}
extension LispToken: Printable {
    var description: String {
        switch self {
        case ParenthesesL:
            return "("
        case ParenthesesR:
            return ")"
        case let .Symbol(symbol):
            return symbol
        case let .Number(number):
            return "\(number)"
        }
    }
}
extension LispToken {
    var isParenthesesL: Bool {
        switch self {
        case .ParenthesesL:
            return true
        default:
            return false
        }
    }
    var isParenthesesR: Bool {
        switch self {
        case ParenthesesR:
            return true
        default:
            return false
        }
    }
    var expr: SExpr? {
        switch self {
        case let .Symbol(symbol):
            return SExpr(Atom(symbol))
        case let .Number(number):
            return SExpr(Atom(number))
        default:
            return nil
        }
    }
}

typealias LispTokenizerSingle = Combinator<Character, LispToken>.Parser
typealias LispTokenizer = Combinator<Character, List<LispToken>>.Parser

let tParenthesesL: LispTokenizerSingle = bind(token("(")) { _ in
    return pure(LispToken.ParenthesesL)
}
let tParenthesesR: LispTokenizerSingle = bind(token(")")) { _ in
    return pure(LispToken.ParenthesesR)
}
let tSymbol: LispTokenizerSingle = bind(NSCharacterSet(charactersInString: "+-*/").parser) { r in
    return pure(LispToken.Symbol(String(r)))
}

let tNumber: LispTokenizerSingle = bind(oneOrMore(NSCharacterSet.decimalDigitCharacterSet().parser)) { r in
    pure(LispToken.Number(NSString(string: String(r)).doubleValue))
}

func ignoreLeadingWhitespace<T>(p: Combinator<Character, T>.Parser) -> Combinator<Character, T>.Parser {
    return bind(zeroOrMore(NSCharacterSet.whitespaceCharacterSet().parser)) { result in
        return p
    }
}

let tLispTokenizer: LispTokenizer = oneOrMore(ignoreLeadingWhitespace(tParenthesesL <|> tParenthesesR <|> tSymbol <|> tNumber))

typealias LispExpressionParser = Combinator<LispToken, SExpr>.Parser

func parentheses<T>(factory: () -> Combinator<LispToken, T>.Parser) -> Combinator<LispToken, T>.Parser {
    return bind(satisfy { t in t.isParenthesesL } ) { L in
        bind(factory()) { insideList in
            bind(satisfy { t in t.isParenthesesR }) { R in
                pure(insideList)
            }
        }
    }
}

func pLispExpression() -> LispExpressionParser {
    let pAtom: LispExpressionParser = bind(consumer()) { r in
        if let expr = r.expr {
            return pure(expr)
        }
        return zero()
    }
    return bind(parentheses { zeroOrMore(pLispExpression() <|> pAtom) }) { exprs in
        pure(SExpr(exprs))
    }
}

//for (r, s) in tLispTokenizer(Array("(* (+ 3 18) 2)").toList) {
//    println("Success, found \(r.toArray), remainder: \(Array(s))")
//}

for (r, s) in tLispTokenizer(Array("(* (+ 3 18) 2)").toList) {
    let e = pLispExpression()
    for (r, s) in e(r) {
        println("Success \(r), eval = \(eval(r))")
    }
}
