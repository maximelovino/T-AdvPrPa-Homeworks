val s = Empty + 3 + 2 + 6 + 1
val s2 = Empty + 3 + 2 + 5 + 4 + 8

s.foreach(println)

s foreach (x => print(s"${x + 1},"))

s.union(s2).foreach(println)
println("==========")
s.intersect(s2).foreach(println)

(s2 - 5 - 3).foreach(println)

val o1 = Empty + 3 + 4 + 12 + 5
val o2 = o1 - 3 - 4
println(o2)

val o3 = o2 + 8 + 7
val o4 = o3 + 10

println(o3 union o4)

println(o3 intersect o4)

val o5 = o4 - 7 - 8
println(o3 intersect o5)