use std::io::{stdout, BufRead, Read, Write};

mod lexer;

enum AbortReason {
    NotEnoughMemory,
}

#[repr(u64)]
enum Tag {
    Atom = 0x7ff8,
    Primitive = 0x7ff9,
    Construct = 0x7ffa, // https://en.wikipedia.org/wiki/Cons
    Closure = 0x7ffb,
    Nil = 0x7ffc,
    Number,
}

impl Into<u64> for Tag {
    fn into(self) -> u64 {
        self as u64
    }
}

impl From<u64> for Tag {
    fn from(v: u64) -> Self {
        match v {
            0x7ff8 => Tag::Atom,
            0x7ff9 => Tag::Primitive,
            0x7ffa => Tag::Construct,
            0x7ffb => Tag::Closure,
            0x7ffc => Tag::Nil,
            _ => Tag::Number,
        }
    }
}

pub struct Primitive<T>
where
    T: Read,
{
    pub symbol: &'static str,
    pub fun: fn(&mut Risp<T>, f64, f64) -> f64,
}

#[inline(always)]
fn _box(t: Tag, v: u64) -> f64 {
    f64::from_bits(((t as u64) << 48) | v)
}

#[inline(always)]
fn _ord(b: f64) -> u64 {
    b.to_bits() & ((!0) >> (64 - 48))
}

#[inline(always)]
fn _tag(b: f64) -> u64 {
    f64::to_bits(b) >> 48
}

#[inline(always)]
fn _num(n: f64) -> f64 {
    n
}

#[inline(always)]
fn _equ(l: f64, r: f64) -> bool {
    f64::to_bits(l) == f64::to_bits(r)
}

#[inline(always)]
fn _not(v: f64) -> bool {
    _tag(v) == (Tag::Nil as u64)
}

const N: usize = 1024;

pub struct Risp<T>
where
    T: Read,
{
    cell: [f64; N],
    stack_p: usize,
    heap_p: usize,
    global_env: f64,

    lexer: lexer::Lexer<T>,

    nil: f64,
    tru: f64,
    err: f64,
}

impl<T: BufRead> Risp<T> {
    pub const PRIMITIVES: [Primitive<T>; 20] = [
        Primitive {
            symbol: "lambda",
            fun: |ctx, boxed, env| {
                let vars = ctx.car(boxed);
                let expr = ctx.car(ctx.cdr(boxed));
                ctx.closure(vars, expr, env)
            },
        },
        Primitive {
            symbol: "define",
            fun: |ctx, boxed, env| {
                let ev = ctx.eval(ctx.car(ctx.cdr(boxed)), env);
                ctx.global_env = ctx.pair(ctx.car(boxed), ev, ctx.global_env);
                ctx.car(boxed)
            },
        },
        Primitive {
            symbol: "not",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                if _not(ctx.car(l)) {
                    ctx.tru
                } else {
                    ctx.nil
                }
            },
        },
        Primitive {
            symbol: "or",
            fun: |ctx, boxed, env| {
                let mut p = boxed;
                while _tag(p) != (Tag::Nil as u64) {
                    let ev = ctx.eval(ctx.car(p), env);

                    if !_not(ev) {
                        return ctx.tru;
                    }

                    p = ctx.cdr(p);
                }
                ctx.nil
            },
        },
        Primitive {
            symbol: "and",
            fun: |ctx, boxed, env| {
                let mut p = boxed;
                while _tag(p) != (Tag::Nil as u64) {
                    let ev = ctx.eval(ctx.car(p), env);

                    if _not(ev) {
                        return ctx.nil;
                    }

                    p = ctx.cdr(p);
                }
                ctx.tru
            },
        },
        Primitive {
            symbol: "int",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                (ctx.car(l) as u64) as f64
            },
        },
        Primitive {
            symbol: "+",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                let mut n = ctx.car(l);
                let mut p = ctx.cdr(boxed);

                while !_not(p) {
                    n += ctx.car(p);
                    p = ctx.cdr(p);
                }

                _num(n)
            },
        },
        Primitive {
            symbol: "-",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                let mut n = ctx.car(l);
                let mut p = ctx.cdr(boxed);

                if _not(p) {
                    return -_num(n);
                }

                loop {
                    n -= ctx.car(p);
                    p = ctx.cdr(p);
                    if _not(p) {
                        break;
                    }
                }

                _num(n)
            },
        },
        Primitive {
            symbol: "*",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                let mut n = ctx.car(l);
                let mut p = ctx.cdr(boxed);

                while !_not(p) {
                    n *= ctx.car(p);
                    p = ctx.cdr(p);
                }

                _num(n)
            },
        },
        Primitive {
            symbol: "/",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                let mut n = ctx.car(l);
                let mut p = ctx.cdr(boxed);

                while !_not(p) {
                    n /= ctx.car(p);
                    p = ctx.cdr(p);
                }

                _num(n)
            },
        },
        Primitive {
            symbol: "quote",
            fun: |ctx, boxed, _env| ctx.car(boxed),
        },
        Primitive {
            symbol: "cons",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                ctx.cons(ctx.car(l), ctx.car(ctx.cdr(l)))
            },
        },
        Primitive {
            symbol: "car",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                ctx.car(ctx.car(l))
            },
        },
        Primitive {
            symbol: "cdr",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                ctx.cdr(ctx.car(l))
            },
        },
        Primitive {
            symbol: ">",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                if ctx.car(l) - ctx.car(ctx.cdr(l)) < 0.0 {
                    ctx.tru
                } else {
                    ctx.nil
                }
            },
        },
        Primitive {
            symbol: "eq?",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                if _equ(ctx.car(l), ctx.car(ctx.cdr(l))) {
                    ctx.tru
                } else {
                    ctx.nil
                }
            },
        },
        Primitive {
            symbol: "cond",
            fun: |ctx, boxed, env| {
                let mut p = boxed;
                while _tag(p) != Tag::Nil.into() {
                    let ev = ctx.eval(ctx.car(ctx.car(p)), env);
                    if !_not(ev) {
                        break;
                    }
                    p = ctx.cdr(p);
                }
                ctx.eval(ctx.car(ctx.cdr(ctx.car(p))), env)
            },
        },
        Primitive {
            symbol: "if",
            fun: |ctx, boxed, env| {
                let block = if _not(ctx.eval(ctx.car(boxed), env)) {
                    ctx.cdr(boxed)
                } else {
                    boxed
                };
                ctx.eval(ctx.car(ctx.cdr(block)), env)
            },
        },
        Primitive {
            symbol: "let",
            fun: |ctx, boxed, env| {
                let mut p = boxed;
                let mut e = env;
                while _tag(p) != Tag::Nil.into() && _tag(ctx.cdr(p)) != Tag::Nil.into() {
                    let ev = ctx.eval(ctx.car(ctx.cdr(ctx.car(p))), e);
                    e = ctx.pair(ctx.car(ctx.car(p)), ev, e);
                    p = ctx.cdr(p);
                }
                ctx.eval(ctx.car(p), e)
            },
        },
        Primitive {
            symbol: "eval",
            fun: |ctx, boxed, env| {
                let l = ctx.evlist(boxed, env);
                ctx.eval(ctx.car(l), env)
            },
        },
    ];

    pub fn new(source: T) -> Risp<T> {
        let mut r = Risp {
            cell: [0.0; N],
            stack_p: N,
            heap_p: 0,
            global_env: 0.0,

            lexer: lexer::Lexer::new(source),

            nil: _box(Tag::Nil, 0),
            tru: 0.0,
            err: 0.0,
        };

        r.load();
        r
    }

    fn load(&mut self) {
        self.tru = self.atom("#t");
        self.err = self.atom("ERR");
        self.global_env = self.pair(self.tru, self.tru, self.nil);

        for (i, p) in (Risp::PRIMITIVES as [Primitive<T>; 20]).iter().enumerate() {
            let atom = self.atom(p.symbol);

            self.global_env = self.pair(atom, _box(Tag::Primitive, i as u64), self.global_env);
        }
    }

    fn abort(&self, reason: AbortReason) {
        let s = match reason {
            AbortReason::NotEnoughMemory => "Not enough memory",
        };

        panic!("ABORT: {}", s);
    }

    fn can_alloc(&self, bytes: usize) -> bool {
        self.heap_p + bytes < (self.stack_p << 3)
    }

    fn byte_at(&self, addr: usize) -> u8 {
        let p = 2 - (addr % 3);
        ((f64::to_bits(self.cell[addr / 3]) & (255 << (8 * p))) >> (8 * p) as u64) as u8
    }

    fn string_at(&self, mut addr: usize) -> String {
        let mut v: Vec<u8> = vec![];
        let mut b = self.byte_at(addr);

        while b != 0 {
            addr += 1;
            v.push(b);
            b = self.byte_at(addr);
        }

        String::from_utf8(v).unwrap()
    }

    fn inc_heap(&mut self, byte: u8) {
        self.cell[self.heap_p / 3] = f64::from_bits(
            f64::to_bits(self.cell[self.heap_p / 3])
                | (byte as u64) << (8 * (2 - (self.heap_p % 3))) as u64,
        );
        self.heap_p += 1;
    }

    fn inc_stack(&mut self, v: f64) {
        self.stack_p -= 1;
        self.cell[self.stack_p] = v;
    }

    fn atom_pos(&self, bytes: &[u8]) -> usize {
        let mut i: usize = 0;
        let mut found: bool;

        while i < self.heap_p {
            found = true;
            for b in bytes {
                if *b != self.byte_at(i) || i + 1 == self.heap_p {
                    found = false;
                    break;
                }
                i += 1;
            }

            if found && self.byte_at(i) == 0 {
                return (i - bytes.len()) as usize;
            }

            while self.byte_at(i) != 0 {
                i += 1;
            }
            i += 1;
        }

        self.heap_p
    }

    /**
     * Get/Add atom `s` from/to the heap
     */
    fn atom(&mut self, s: &str) -> f64 {
        let bytes = s.as_bytes();
        let pos = self.atom_pos(bytes);

        if pos == self.heap_p {
            if !self.can_alloc(bytes.len()) {
                self.abort(AbortReason::NotEnoughMemory)
            }

            for b in bytes {
                self.inc_heap(*b);
            }
            self.inc_heap(0);
        }

        _box(Tag::Atom, pos as u64)
    }

    /**
     * Add the pair (l, r) to the stack
     */
    fn cons(&mut self, l: f64, r: f64) -> f64 {
        if !self.can_alloc(std::mem::size_of::<f64>() * 2) {
            self.abort(AbortReason::NotEnoughMemory)
        }

        self.inc_stack(l);
        self.inc_stack(r);

        _box(Tag::Construct, self.stack_p as u64)
    }

    /**
     * Contents of the Address part of Register
     *
     * > (car (cons 'a 'b))
     * a
     * > (car (cons 'a (cons 'b (cons 'c ()))))
     * a
     */
    fn car(&self, pointer: f64) -> f64 {
        if (_tag(pointer) & !((Tag::Construct as u64) ^ (Tag::Closure as u64)))
            == (Tag::Construct as u64)
        {
            self.cell[(_ord(pointer) + 1) as usize]
        } else {
            self.err
        }
    }

    /**
     * Contents of the Decrement part of Register
     *
     * > (cdr (cons 'a 'b))
     * b
     * > (cdr (cons 'a (cons 'b (cons 'c ()))))
     * (b c)
     */
    fn cdr(&self, pointer: f64) -> f64 {
        if (_tag(pointer) & !((Tag::Construct as u64) ^ (Tag::Closure as u64)))
            == (Tag::Construct as u64)
        {
            self.cell[(_ord(pointer)) as usize]
        } else {
            self.err
        }
    }

    /**
     * Create a pair with context
     */
    fn pair(&mut self, l: f64, r: f64, env: f64) -> f64 {
        let p = self.cons(l, r);
        self.cons(p, env)
    }

    /**
     * Create a special `pair` that can have global context
     */
    fn closure(&mut self, l: f64, r: f64, env: f64) -> f64 {
        let cenv = if _equ(env, self.global_env) {
            self.nil
        } else {
            env
        };
        _box(Tag::Closure, _ord(self.pair(l, r, cenv)))
    }

    /**
     * Search the context for the `atom` associated value
     */
    fn assoc(&self, atom: f64, mut env: f64) -> f64 {
        while _tag(env) == (Tag::Construct as u64) && !_equ(atom, self.car(self.car(env))) {
            env = self.cdr(env);
        }

        if _tag(env) == (Tag::Construct as u64) {
            self.cdr(self.car(env))
        } else {
            self.err
        }
    }

    fn eval(&mut self, boxed: f64, env: f64) -> f64 {
        match Tag::from(_tag(boxed)) {
            Tag::Atom => self.assoc(boxed, env),
            Tag::Construct => {
                let fun = self.eval(self.car(boxed), env);
                self.apply(fun, self.cdr(boxed), env)
            }
            _ => boxed,
        }
    }

    fn apply(&mut self, fun: f64, args: f64, env: f64) -> f64 {
        match Tag::from(_tag(fun)) {
            Tag::Primitive => (Risp::PRIMITIVES[_ord(fun) as usize].fun)(self, args, env),
            Tag::Construct => self.reduce(fun, args, env),
            _ => self.err,
        }
    }

    fn reduce(&mut self, fun: f64, boxed: f64, env: f64) -> f64 {
        let v = self.cdr(self.car(boxed));
        let benv = if _not(self.cdr(fun)) {
            self.global_env
        } else {
            self.cdr(fun)
        };
        let evargs = self.evlist(boxed, env);
        let e = self.bind(self.car(self.car(fun)), evargs, benv);

        self.eval(v, e)
    }

    fn evlist(&mut self, boxed: f64, env: f64) -> f64 {
        if _tag(boxed) == Tag::Construct.into() {
            let element = self.eval(self.car(boxed), env);
            let rest = self.evlist(self.cdr(boxed), env);

            self.cons(element, rest)
        } else {
            self.eval(boxed, env)
        }
    }

    fn bind(&mut self, vars: f64, evargs: f64, env: f64) -> f64 {
        match Tag::from(_tag(vars)) {
            Tag::Nil => env,
            Tag::Construct => {
                let benv = self.pair(self.car(vars), self.car(evargs), env);
                self.bind(self.cdr(vars), self.cdr(evargs), benv)
            }
            _ => self.pair(vars, evargs, env),
        }
    }

    fn list(&mut self) -> f64 {
        let token = self.lexer.next();

        if token.is_some() {
            match token.unwrap() {
                lexer::Token::Close => self.nil,
                lexer::Token::Pair => {
                    let x = self.read();
                    self.lexer.next();
                    x
                }
                v => {
                    let l = self.parse(v);
                    let r = self.list();
                    self.cons(l, r)
                }
            }
        } else {
            self.err
        }
    }

    fn quote(&mut self) -> f64 {
        let a = self.atom("quote");
        let l = self.read();
        let v = self.cons(l, self.nil);
        self.cons(a, v)
    }

    fn parse(&mut self, token: lexer::Token) -> f64 {
        match token {
            lexer::Token::Open => self.list(),
            lexer::Token::Quote => self.quote(),
            lexer::Token::Number(n) => n,
            lexer::Token::Atom(a) => self.atom(a.as_str()),
            _ => self.err,
        }
    }

    fn read(&mut self) -> f64 {
        match self.lexer.next() {
            Some(token) => self.parse(token),
            None => self.nil,
        }
    }

    fn gc(&mut self) {
        self.stack_p = _ord(self.global_env) as usize;
    }

    pub fn run(&mut self) -> f64 {
        self.lexer.iter();
        let value = self.read();
        let ret = self.eval(value, self.global_env);
        self.gc();
        ret
    }

    pub fn input(&self) {
        print!("> ");
        stdout().flush().unwrap();
    }

    fn printls(&self, mut v: f64) {
        print!("(");

        loop {
            self.print(self.car(v));
            v = self.cdr(v);
            if _not(v) {
                break;
            }
            if _tag(v) == Tag::Construct.into() {
                print!(" . ");
                self.print(v);
                break;
            }
            print!(" ");
        }

        print!(")");
    }

    pub fn print(&self, v: f64) {
        match Tag::from(_tag(v)) {
            Tag::Nil => println!("()"),
            Tag::Atom => println!("{}", self.string_at(_ord(v) as usize)),
            Tag::Primitive => println!(
                "<{}>",
                (Risp::PRIMITIVES as [Primitive<T>; 20])[_ord(v) as usize].symbol
            ),
            Tag::Construct => self.printls(v),
            Tag::Closure => println!("{{{}}}", _ord(v)),
            Tag::Number => println!("{}", v),
        }
    }

    pub fn _printme(&self) {
        println!("STACK[{}]", self.stack_p);
        for i in 0..(N - self.stack_p) {
            let v = self.cell[N - i - 1];
            println!("[{}] TAG[{}] ORD[${}]", N - i - 1, _tag(v), _ord(v));
        }

        println!("\nHEAP[{}]", self.heap_p);
        for i in 0..self.heap_p {
            println!("[{}] {}", i, self.byte_at(i));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nan_boxing() {
        let v: u64 = 32;
        let b: f64 = _box(Tag::Construct, v);
        let t: u64 = Tag::Construct.into();

        assert!(b.is_nan());
        assert_eq!(v, _ord(b));
        assert_eq!(t, _tag(b));
    }

    #[test]
    fn risp_init() {
        let risp = Risp::new(&b""[..]);

        assert!(risp.atom_pos(b"#t") < risp.heap_p);
        assert!(risp.atom_pos(b"ERR") < risp.heap_p);
    }

    #[test]
    fn risp_atom() {
        let mut risp = Risp::new(&b""[..]);

        assert_eq!(_ord(risp.atom("abcde")) as usize, risp.atom_pos(b"abcde"));
        assert_eq!(_ord(risp.atom("abc")) as usize, risp.atom_pos(b"abc"));
        assert!(risp.atom_pos(b"abcde") < risp.atom_pos(b"abc"));
    }

    #[test]
    #[should_panic]
    fn risp_not_enogh_memory_panic() {
        let mut risp = Risp::new(&b""[..]);
        let mut full = String::from("");

        for _ in 0..(N << 3) {
            full += "x";
        }

        risp.atom(full.as_str());
    }
}
