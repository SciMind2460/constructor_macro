#![no_std]
extern crate alloc;

pub use macros::Constructor;

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Constructor)]
    struct MyUnit;

    #[derive(Constructor)]
    struct MyUnnamed(i32, i32);

    #[derive(Constructor)]
    #[ctor_name = "random"]
    struct MyNamed {
        #[ctor_rename = "holy"]
        stuff: i32,
        unstuff: i32
    }

    #[test]
    fn it_works_named() {
        let _named = MyNamed::random(3, 4);
    }

    #[test]
    fn it_works_unnamed() {
        let _unnamed = MyUnnamed::new(3, 4);
    }

    #[test]
    fn it_works_unit() {
        let _unit = MyUnit::new();
    }
}