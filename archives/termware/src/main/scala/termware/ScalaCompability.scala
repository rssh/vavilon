package termware

object ScalaCompability {


  def existsTrue(t:MultiTerm): Boolean =
  {
    t.multiKind match {
      case MultiKind.Empty(e) => false
      case MultiKind.Contradiction(ct) => false
      case MultiKind.Star(s) => false // impossible.
      case MultiKind.Set(s) => ! s.find(asBoolean).isEmpty
      case MultiKind.SeqOr(s) => ! s.find(existsTrue).isEmpty
      case MultiKind.Point(pt) => asBoolean(pt)
    }
  }

  def asBoolean(t:PointTerm):Boolean =
    t.pointKind match {
      case PointKind.Primitive(p) =>
        if (p.primitiveTypeIndex == BooleanTermOps.primitiveTypeIndex ) {
          p.valueAs[Boolean]
        } else {
          false
        }
      case _ => false
    }

}
