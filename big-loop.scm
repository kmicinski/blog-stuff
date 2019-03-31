; Church encoding of: 
; (letrec [(f (lambda (x) (if (zero? x) 0 (f (sub1 x)))))] (f 25))
((((((((((((λ (Y-comb)
              (λ (church:null?)
                (λ (church:cons)
                  (λ (church:car)
                    (λ (church:cdr)
                      (λ (church:add1)
                        (λ (church:sub1)
                          (λ (church:*)
                            (λ (church:+)
                              (λ (church:zero?)
                                (λ (church:not)
                                  ((λ (f)
                                     (f
                                      (λ (f)
                                        (λ (x)
                                          (f
                                           (f
                                            (f
                                             (f
                                              (f
                                               (f
                                                (f
                                                 (f
                                                  (f
                                                   (f
                                                    (f
                                                     (f
                                                      (f
                                                       (f
                                                        (f
                                                         (f
                                                          (f
                                                           (f
                                                            (f
                                                             (f
                                                              (f
                                                               (f
                                                                (f
                                                                 (f
                                                                  (f
                                                                   x)))))))))))))))))))))))))))))
                                   (Y-comb
                                    (λ (f)
                                      (λ (x)
                                        (((church:zero? x)
                                          (λ (_)
                                            (λ (f) (λ (x) x))))
                                         (λ (_)
                                           (f (church:sub1 x)))))))))))))))))))
            ((λ (u) (u u))
             (λ (y) (λ (mk) (mk (λ (x) (((y y) mk) x)))))))
           (λ (p)
             ((p
               (λ (a)
                 (λ (b) (λ (tt) (λ (ft) (ft (λ (_) _)))))))
              (λ (_) (λ (tt) (λ (ft) (tt (λ (_) _))))))))
          (λ (a)
            (λ (b)
              (λ (when-cons) (λ (when-null) ((when-cons a) b))))))
         (λ (p)
           ((p (λ (a) (λ (b) a))) (λ (_) (λ (x) x)))))
        (λ (p)
          ((p (λ (a) (λ (b) b))) (λ (_) (λ (x) x)))))
       (λ (n0) (λ (f) (λ (x) (f ((n0 f) x))))))
      (λ (n0)
        (λ (f)
          (λ (y)
            (((n0 (λ (g) (λ (h) (h (g f))))) (λ (_) y))
             (λ (x) x))))))
     (λ (n0) (λ (n1) (λ (f) (λ (x) ((n0 (n1 f)) x))))))
    (λ (n0) (λ (n1) (λ (f) (λ (x) ((n1 f) ((n0 f) x)))))))
   (λ (n0)
     ((n0 (λ (b) (λ (tt) (λ (ft) (ft (λ (_) _))))))
      (λ (tt) (λ (ft) (tt (λ (_) _)))))))
  (λ (bool)
    ((bool (λ (_) (λ (tt) (λ (ft) (ft (λ (_) _))))))
     (λ (_) (λ (tt) (λ (ft) (tt (λ (_) _))))))))
