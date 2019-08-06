pub mod demo1;
pub mod test2;
pub mod test3;
pub mod test5;
pub mod test6;
pub mod test7;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
