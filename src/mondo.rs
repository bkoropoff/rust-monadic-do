#![crate_type = "rlib"]
#![feature(macro_rules,globs)]

#[macro_export]
macro_rules! monad(
    ($m:ident { 
        let $p:pat <- $e:expr;
        $(let $p_rest:pat <- $e_rest:expr;)*
    } in $y:expr) => (
        $m::mbind($e,|$p| {
            monad!($m {
                $(let $p_rest <- $e_rest;)*
            } in $y)
        }));
    ($m:ident { } in $y:expr) => ($m::mpure($y));
)

pub struct OptionMonad;

impl OptionMonad {
    pub fn mpure<T>(value: T) -> Option<T> { Some(value) }
    pub fn mbind<T,U>(value: Option<T>, func: |T| -> Option<U>) -> Option<U> {
        match value {
            Some(v) => func(v),
            None => None
        }
    }
}

pub struct ResultMonad;

impl ResultMonad {
    pub fn mpure<T,E>(value: T) -> Result<T,E> { Ok(value) }
    pub fn mbind<T,U,E>(value: Result<T,E>, func: |T| -> Result<U,E>) -> Result<U,E> {
        match value {
            Ok(v) => func(v),
            Err(e) => Err(e)
        }
    }
}

pub struct IterMonad;

impl IterMonad {
    pub fn mpure<T>(value: T) -> std::option::Item<T> {
        Some(value).move_iter()
    }
    pub fn mbind<T,U,I:Iterator<T>,J:Iterator<U>>(value: I, func: |T| -> J)
                                                  -> std::vec::MoveItems<U> {
        value.flat_map(func).collect::<Vec<U>>().move_iter()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn option() {
        let result = monad!(OptionMonad {
            let (a,x) <- Some((2i,6i));
            let (b,y) <- Some((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, Some((2+3,6*7)));
    }

    #[test]
    fn option_fail() {
        let result = monad!(OptionMonad {
            let (a,x) <- None::<(int,int)>;
            let (b,y) <- Some((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, None);
    }

    #[test]
    fn result() {
        let result: Result<(int,int),()> = monad!(ResultMonad {
            let (a,x) <- Ok((2i,6i));
            let (b,y) <- Ok((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, Ok((2+3,6*7)));
    }

    #[test]
    fn result_fail() {
        let result: Result<(int,int),()> = monad!(ResultMonad {
            let (a,x) <- Err::<(int,int),()>(());
            let (b,y) <- Ok((3i,7i));
        } in (a+b,x*y));
        
        assert_eq!(result, Err(()));
    }

    #[test]
    fn iter() {
        let mut result = monad!(IterMonad {
            let a <- vec![1u,2,3].move_iter();
            let b <- vec!["hello"].move_iter();
        } in (a,b));

        assert_eq!(result.collect::<Vec<(uint,&'static str)>>(),
                   vec![(1u,"hello"),(2,"hello"),(3,"hello")]);
    }
}
