package chapter2.Ex2_2

object N1 {
  /*A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,E,I,N,O,Q,S,S,T,U,Y*/
}

object N2 {
  /*E,A,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Q,Y,U,E,S,T,I,O,N
  A,E,S,Q,U,Y,E,S,T,I,O,N
  A,E,Q,S,U,Y,E,S,T,I,O,N
  A,E,Q,S,U,Y,E,S,T,I,O,N
  A,E,Q,S,U,Y,E,S,T,I,O,N
  A,E,Q,S,U,Y,E,S,T,I,O,N
  A,E,Q,S,U,Y,E,S,T,I,N,O
  A,E,Q,S,U,Y,E,I,N,O,S,T
  A,E,E,I,N,O,Q,S,S,T,U,Y*/
}

object N3 {
  /*E,A,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Y,Q,U,E,S,T,I,O,N
  A,E,S,Y,Q,U,E,S,I,T,O,N
  A,E,S,Y,Q,U,E,S,I,T,N,O
  A,E,S,Y,Q,U,E,S,I,T,N,O
  A,E,S,Y,E,Q,S,U,I,T,N,O
  A,E,S,Y,E,Q,S,U,I,N,O,T
  A,E,E,Q,S,S,U,Y,I,N,O,T
  A,E,E,I,N,O,Q,S,S,T,U,Y*/
}

object N4 {
  /**
   * 是，merge 方法必须对两个有序数组进行操作，因为每次从左右比较选择一个值时，暗含了这个值必须必尚未选择的值都小，因此
   * 数组必须是有序的。
   */
}

object N5 {
  /**
   * 对于 N39 而言，自顶向下的方法每次并归子数组的大小依次为 1，2，4，8，16，32
   * 对于自下而上的方法，则一致，但是执行顺序存在差异，自下而上对所有同一 step 的并归完全进行再开始更大 step 并归
   * 而自顶向下的方法则使用栈来实现，因此其先处理了前一半，再处理后一半。每个 step 都先后处理了两次。
   */
}
