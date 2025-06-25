pub trait Assign {
    type Elem;

    fn assign(&mut self, i: usize) -> &mut Self::Elem;
}
impl<T: Default> Assign for Vec<T> {
    type Elem = T;

    fn assign(&mut self, i: usize) -> &mut Self::Elem {
        if self.len() <= i {
            self.resize_with(i+1, Default::default);
        }
        &mut self[i]
    }
}
