import quickcheck._

val qch = new QuickCheckHeap with BinomialHeap
val heap = qch.insert(1, qch.empty)
val testHeap = qch.genHeap.sample.get
val props = qch.properties.toMap
val meld5 = props("Heap.meld5")
meld5.check
