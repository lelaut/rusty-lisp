use std::io::{BufRead, Read};

pub struct Lexer<T>
where
    T: Read,
{
    reader: T,
    buf: [u8; 256],
    size: usize,
    pos: usize,
}

impl<T: Read> Lexer<T> {
    pub fn new(source: T) -> Lexer<T>
    where
        T: BufRead,
    {
        Lexer {
            reader: source,
            buf: [0; 256],
            size: 0,
            pos: 0,
        }
    }

    pub fn iter(&mut self) -> &mut Self {
        self.pos = 0;
        self.size = self.reader.read(&mut self.buf).expect("Unable to read");
        self
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(f64),
    Open,
    Close,
    Quote,
    Atom(String),
    Pair,
}

impl<T: Read> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.buf[self.pos] == b'\n' || self.buf[self.pos] == b'\r' {
            while self.pos < self.size
                && (self.buf[self.pos] == b'\n' || self.buf[self.pos] == b'\r')
            {
                self.pos += 1;
            }
            return None;
        }

        while self.pos < self.size && self.buf[self.pos] == b' ' {
            self.pos += 1;
        }

        let mut j = self.pos;
        let token: Option<Self::Item> = if j < self.size {
            j += 1;

            match self.buf[j - 1] {
                b'(' => Some(Token::Open),
                b')' => Some(Token::Close),
                b'\'' => Some(Token::Quote),
                b'.' => Some(Token::Pair),
                _ => {
                    j -= 1;
                    while j < self.size {
                        if self.buf[j] == b'('
                            || self.buf[j] == b')'
                            || self.buf[j] == b'\''
                            || self.buf[j] == b' '
                            || self.buf[self.pos] == b'\n'
                            || self.buf[self.pos] == b'\r'
                        {
                            break;
                        }
                        j += 1
                    }

                    let slice = std::str::from_utf8(&self.buf[self.pos..j]).unwrap();
                    match slice.parse::<f64>() {
                        Ok(v) => Some(Token::Number(v)),
                        _ => Some(Token::Atom(slice.to_string())),
                    }
                }
            }
        } else {
            None
        };

        self.pos = j;

        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_some_exprs() {
        let text = b"(lambda 1 . 232.32 'a ())\r\n";
        let expected = [
            Token::Open,
            Token::Atom("lambda".to_string()),
            Token::Number(1.0),
            Token::Pair,
            Token::Number(232.32),
            Token::Quote,
            Token::Atom("a".to_string()),
            Token::Open,
            Token::Close,
            Token::Close,
        ];
        let mut lexer = Lexer::new(&text[..]);
        let mut i: usize;

        i = 0;
        for received in lexer.iter() {
            assert!(i < expected.len());
            assert_eq!(received, expected[i]);
            i += 1;
        }
        assert_eq!(i, expected.len());
    }

    #[test]
    fn test_empty_expr() {
        let text = b"";
        let mut lexer = Lexer::new(&text[..]);

        assert_eq!(lexer.iter().next(), None);
    }
}
