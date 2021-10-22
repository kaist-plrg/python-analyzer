package kr.ac.kaist.pyanalyzer.util

object MultiMap {
  type MultiMap[K, V] = Map[K, Set[V]]

  implicit class I[K, V](map: MultiMap[K, V]){
    def addOne(k: K, v: V): MultiMap[K, V] = map.get(k) match {
      case None => map + (k -> Set(v)) 
      case Some(vs) => map + (k -> (vs + v))
    }
    def addKey(k: K): MultiMap[K, V] = map.get(k) match {
      case None => map + (k -> Set())
      case Some(_) => map
    }
    def merge(map2: MultiMap[K, V]): MultiMap[K, V] ={
      (map.keys ++ map2.keys).map(k => {
        (k, map.getOrElse(k, Set[V]()) ++ map2.getOrElse(k, Set[V]()))
      }).toMap
    }
  }
  
  //implicit def i[K, V](map: MultiMap[K, V]): I[K, V] = I[K, V](map)
}


