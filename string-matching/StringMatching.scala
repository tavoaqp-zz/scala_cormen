object StringMatching {
  import scala.collection.mutable.HashMap


  def rubin_karp(T:String, P:String, d:Int, q:Int) {
    val n=T.size
    val m=P.size
    val h:Int=scala.math.pow(d,m-1).toInt % q
    var p=0
    var t=Array.fill(n-m+1)(0)
    for (i <- 0 to m-1) {
      p=(d*p + P(i).asDigit) % q
      t(0)=(d*t(0) + T(i).asDigit) % q
    }
    for (s <- 0 to n-m) {
      if (p==t(s)) {
        println(s"T.substring(s,s+m)=${T.substring(s,s+m)} P=$P")
        if (P==T.substring(s,s+m)) {
          print(s"Pattern occurs with shift $s")
        }
      }
      if (s<n-m) {        
        val baseValue=d*(t(s)-T(s).asDigit*h) + T(s+m).asDigit
        val nextValue= (baseValue % q)+(if (baseValue < 0) q else 0)
        t(s+1)=nextValue
      }
    }
  }

  def compute_transition(P:String, alpha:Set[String]):HashMap[(Int,String),Int]={
    var trans=HashMap[(Int,String),Int]()
    val m=P.length
    for (q <- 0 to m) {
      println(s"q=$q")
      for (a <- alpha) {
        var k=scala.math.min(m+1,q+2)
        
        var qaString=""
        var kString=""
        do {
          k=k-1
          qaString=P.substring(0,q)+a
          kString=P.substring(0,k)
        }
        while(!qaString.endsWith(kString))

        val newEntry=(q,a)->k
        trans+=newEntry
      }
    }
    trans
  }

  def automaton(T:String, P:String, m:Int) {
     
    val alpha=Range('a','z').toList.map(_.toChar.toString).toSet
    val trans=compute_transition(P,alpha)

    val n=T.length

    var q=0
    for (i <- 1 to n) {
      val pair=(q,T(i-1).toString)
      
      q=trans(pair)
      if (q==m) {
        println(s"Pattern occurs with shift ${i-m}")
        return
      }
    }    
  }

  def kmp(P:String, T:String) ={
    val n=T.size
    val m=P.size
    val pi=compute_prefix_function(P)
    var q=0
    for (i <- 0 to n-1) {
      while (q>0 && P(q)!=T(i)) {
        q=pi(q)
      }
      if (P(q)==T(i)) {
        q=q+1
      }
      if (q==m) {
        println(s"Pattern occurs with shift ${i-m+1}")
        q=pi(q-1)
      }
    }

  }

  def compute_prefix_function(P:String)={
    val m=P.size
    val pi=Array.fill(m)(0)
    var k = 0
    for (q <- 1 to m-1) {
      while (k>0 && P(k)!=P(q)) {
        k=pi(k)
      }
      if (P(k)==P(q)) {
        k=k+1
      
      pi(q)=k
    }
    pi
  }


}
