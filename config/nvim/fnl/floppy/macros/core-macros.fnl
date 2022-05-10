(fn nil? [x]
  (= nil x))

(fn str? [x]
  (= :string (type x)))

(fn tbl? [x]
  (= :table (type x)))

(fn ->str [x]
  (tostring x))

(fn ->num [x]
  (tonumber x))

(fn num? [x]
  "checks if 'x' is of number type."
  `(= :number (type ,x)))

(fn odd? [x]
  "checks if 'x' is mathematically of odd parity ;}"
  `(and ,(num? x) (= 1 (% ,x 2))))

(fn even? [x]
  "checks if 'x' is mathematically of even parity ;}"
  `(and ,(num? x) (= 0 (% ,x 2))))

{: nil? : str? : ->str : tbl? : num? : ->num : odd? : even?}
