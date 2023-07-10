use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct Arena<T> {
    elements: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            elements: Vec::default(),
        }
    }

    pub fn allocate(&mut self, t: T) -> Handle<T> {
        self.elements.push(t);
        Self::make_handle(self.elements.len())
    }

    pub fn get(&self, handle: Handle<T>) -> &T {
        self.elements
            .get(
                Self::index_from_handle(handle)
                    .expect("dummy handle should not be used to get an element"),
            )
            .unwrap()
    }

    pub fn get_mut(&mut self, handle: Handle<T>) -> &mut T {
        self.elements
            .get_mut(
                Self::index_from_handle(handle)
                    .expect("dummy handle should not be used to get an element"),
            )
            .unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Handle<T>, &T)> {
        self.elements
            .iter()
            .enumerate()
            .map(|(i, el)| (Self::make_handle(i), el))
    }

    fn make_handle(element_index: usize) -> Handle<T> {
        let id: u32 = element_index.try_into().unwrap();
        Handle {
            id,
            _ghost: PhantomData,
        }
    }

    fn index_from_handle(handle: Handle<T>) -> Option<usize> {
        // Should return None on dummy handle
        (handle.id as usize).checked_sub(1)
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Handle<T> {
    id: u32,
    _ghost: PhantomData<*const T>,
}

impl<T> Handle<T> {
    pub fn dummy() -> Self {
        Self {
            id: 0,
            _ghost: PhantomData,
        }
    }
}

impl<T> std::fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Handle").field("id", &self.id).finish()
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            _ghost: self._ghost,
        }
    }
}
impl<T> Copy for Handle<T> {}
impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}
impl<T> Eq for Handle<T> {}
impl<T> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}
impl<T> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl<T> std::hash::Hash for Handle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}
