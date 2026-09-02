
fn init {
    [1, 2, 3][0]
    [1, 2, 3].op_get(1)
    let a = [1, 2, 3]
    a[0]
    "123"[0]


    a[0] = 1
    a.op_set(0, 1)

    a[1:2]
    a.op_as_view(1, 2)

    T::op_get()
    T::op_set()
    T::op_as_view()
}

fn T::op_get() {}
fn T::op_set() {}
fn T::op_as_view() {}
