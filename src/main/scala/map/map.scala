package group

case class GroupElement[T](g: Group[T], value:T) {
    def *(that: GroupElement[T]): GroupElement[T] = g.add(this, that)
    def inv: GroupElement[T] = g.inv(this)
}

case class Group[T]( set: Set[T], _add : (T,T)=>T, _zero: T, _inv: T=>T ) {
    def get(x: T): GroupElement[T] = GroupElement(this, x)
    def contains(x: T): Boolean = set.contains(x)

    def add(a: GroupElement[T], b: GroupElement[T]): GroupElement[T] = GroupElement(this, _add(a.value, b.value))
    def inv(x: GroupElement[T]): GroupElement[T] = GroupElement(this, _inv(x.value))

    def apply( x: GroupElement[T]) : T = {
        if( this == x.g ) x.value
        else throw new Error("群演算のスコープ内で違う群が使用されています")
    }
}

object Group {
    def _assocLaw[T](f: (T, T)=> T)( x: T, y: T, z: T ): Boolean = f(x, f(y, z)) == f( f(x, y), z)
    def _unitLaw[T](f: (T, T)=> T) (zero: T) (x: T) : Boolean = f( zero, x) == f( x, zero) && f(x, zero) == x
    def _invLaw[T](f: (T, T)=>T)( zero: T)( inv: T=> T)(x: T) : Boolean = f(x, inv(x)) == f( inv(x), x) && f( inv(x), x) == zero

    def assocLaw[T](set: Set[T])(f: (T, T)=> T): Boolean = set.forall( x => set.forall(y => set.forall(z => _assocLaw(f)(x, y, z))))
    def unitLaw[T](set: Set[T])(f: (T, T)=>T)(zero: T) = set.forall( x => _unitLaw(f)(zero)(x))
    def invLaw[T](set: Set[T])(f: (T, T)=>T)(zero: T)(inv: T=> T) = set.forall( x => _invLaw(f)(zero)(inv)(x))

    def apply[T](set: Set[T], add: (T, T)=>T, zero: T, inv: T=>T): Group[T] = {
        if( assocLaw(set)(add) && unitLaw(set)(add)(zero) && invLaw(set)(add)(zero)(inv)) new Group[T](set, add, zero, inv)
        else throw new Error("群の法則が満たされません")
    }
}
