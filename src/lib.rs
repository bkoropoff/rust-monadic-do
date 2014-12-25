#![crate_type = "rlib"]
#![feature(macro_rules, globs)]

#[macro_export]
macro_rules! monad {
    ($m:ident {
        let $p:pat = $e:expr;
        $($rest:tt)*
    } in $y:expr) => ({
        let $p = $e;
        monad!($m { $($rest)* } in $y)
    });
    ($m:ident { 
        $p:pat <- $e:expr;
        $($rest:tt)*
    } in $y:expr) => (
        $m::mbind($e, move |&mut:$p| {
            monad!($m {
                $($rest)*
            } in $y)
        })
    );
    ($m:ident {
        $e:expr;
        $($rest:tt)*
    } in $y:expr) => (
        monad!($m { _ <- $e; $($rest)* } in $y)
    );
    ($m:ident {
        $e:expr
    } in $y:expr) => (
        monad!($m { _ <- $e; } in $y)
    );
    ($m:ident { } in $y:expr) => ($m::mpure($y));
}

#[allow(missing_copy_implementations)]
pub struct OptionMonad;

impl OptionMonad {
    pub fn mpure<T>(value: T) -> Option<T> { Some(value) }
    pub fn mbind<T,U,F>(value: Option<T>, mut func: F) -> Option<U>
                       where F: FnMut(T) -> Option<U> {
        match value {
            Some(v) => func(v),
            None => None
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct ResultMonad;

impl ResultMonad {
    pub fn mpure<T,E>(value: T) -> Result<T,E> { Ok(value) }
    pub fn mbind<T,U,E,F>(value: Result<T,E>, mut func: F) -> Result<U,E> 
                         where F: FnMut(T) -> Result<U,E> {
        match value {
            Ok(v) => func(v),
            Err(e) => Err(e)
        }
    }
}

pub struct IterBind<T,U,I,J,F>
    where I: Iterator<T>,
          J: Iterator<U>,
          F: FnMut(T) -> J {
    input: I,
    output: Option<J>,
    func: F
}

impl<T,U,I,J,F> Iterator<U> for IterBind<T,U,I,J,F>
    where I: Iterator<T>,
          J: Iterator<U>,
          F: FnMut(T) -> J {
    fn next(&mut self) -> Option<U> {
        if let Some(mut o) = self.output.take() {
            if let Some(v) = o.next() {
                self.output = Some(o);
                Some(v)
            } else {
                self.output = self.input.next().map(|v| (self.func)(v));
                self.next()
            }
        } else {
            None
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct IterMonad;

impl IterMonad {
    pub fn mpure<T>(value: T) -> std::option::IntoIter<T> {
        Some(value).into_iter()
    }
    pub fn mbind<T,U,I,J,F>(mut input: I, mut func: F) -> IterBind<T,U,I,J,F>
                           where I: Iterator<T>,
                                 J: Iterator<U>,
                                 F: FnMut(T) -> J {
        let output = input.next().map(|v| func(v));
        IterBind {
            input: input,
            output: output,
            func: func
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn option() {
        let result = monad!(OptionMonad {
            (a,x) <- Some((2i,6i));
            (b,y) <- Some((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, Some((2+3,6*7)));
    }

    #[test]
    fn option_fail() {
        let result = monad!(OptionMonad {
            (a,x) <- None::<(int,int)>;
            (b,y) <- Some((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, None);
    }

    #[test]
    fn result() {
        let result: Result<(int,int),()> = monad!(ResultMonad {
            (a,x) <- Ok((2i,6i));
            (b,y) <- Ok((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, Ok((2+3,6*7)));
    }

    #[test]
    fn result_fail() {
        let result: Result<(int,int),()> = monad!(ResultMonad {
            (a,x) <- Err::<(int,int),()>(());
            (b,y) <- Ok((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, Err(()));
    }

    #[test]
    fn iter() {
        let result = monad!(IterMonad {
            a <- vec![1u, 2, 3].into_iter();
            b <- vec!["hello", "world"].into_iter();
        } in (a,b));

        assert_eq!(result.collect::<Vec<_>>(),
                   vec![(1, "hello"), (1, "world"),
                        (2, "hello"), (2, "world"),
                        (3, "hello"), (3, "world")]);
    }

    #[test]
    fn ordinary_bindings() {
        let result = monad!(OptionMonad {
            let a = Some(5u);
            x <- a;
            let b = Some(4u);
            y <- b;
        } in x*y);
        assert_eq!(result, Some(20));
    }

    #[test]
    fn sequence() {
        let result = monad!(OptionMonad {
            Some(());
            x <- Some(());
            None::<()>
        } in x);
        assert_eq!(result, None);
    }
}
