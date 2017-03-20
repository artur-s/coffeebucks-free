module ReactiveDomain.Aggregate
    

type Version = int
type 't Versioned = Versioned of 't * Version

type Versioned with
    static member Create (t:'t, ver:Version) = Versioned (t, ver)
