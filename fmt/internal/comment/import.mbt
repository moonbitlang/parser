///|
using @basic {type Location, type Position}

///|
using @tokens {type TokenKind}

///|
using @pp {trait Pretty, space, text, pretty}

///|
type Triple = (@tokens.Token, @basic.Position, @basic.Position)

///|
using @syntax {trait IterVisitor}
