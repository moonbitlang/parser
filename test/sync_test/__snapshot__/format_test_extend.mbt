#deprecated
pub extend Type with Trait::{f, g}

extend @pkg.Type with @pkg.Trait::{f as g}

extend &Obj with Trait::{m}

extend Type with Trait::{many_many as many_many_many, many as many_many_many_many, many_many_many as names}
