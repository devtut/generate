---
metaTitle: "Java - Functional Interfaces"
description: "List of standard Java Runtime Library functional interfaces by signature"
---

# Functional Interfaces


In Java 8+, a **functional interface** is an interface that has just one abstract method (aside from the methods of Object). See JLS [§9.8. Functional Interfaces](https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.8).



## List of standard Java Runtime Library functional interfaces by signature


|Parameter Types|Return Type|Interface
|---|---|---|---|---|---|---|---|---|---
|()|void|[Runnable](https://docs.oracle.com/javase/8/docs/api/java/lang/Runnable.html)
|()|T|[Supplier](https://docs.oracle.com/javase/8/docs/api/java/util/function/Supplier.html)
|()|boolean|[BooleanSupplier](https://docs.oracle.com/javase/8/docs/api/java/util/function/BooleanSupplier.html)
|()|int|[IntSupplier](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntSupplier.html)
|()|long|[LongSupplier](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongSupplier.html)
|()|double|[DoubleSupplier](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoubleSupplier.html)
|(T)|void|[Consumer<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/Consumer.html)
|(T)|T|[UnaryOperator<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/UnaryOperator.html)
|(T)|R|[Function<T,R>](https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html)
|(T)|boolean|[Predicate<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/Predicate.html)
|(T)|int|[ToIntFunction<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ToIntFunction.html)
|(T)|long|[ToLongFunction<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ToLongFunction.html)
|(T)|double|[ToDoubleFunction<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ToDoubleFunction.html)
|(T, T)|T|[BinaryOperator<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/BinaryOperator.html)
|(T, U)|void|[BiConsumer<T,U>](https://docs.oracle.com/javase/8/docs/api/java/util/function/BiConsumer.html)
|(T, U)|R|[BiFunction<T,U,R>](https://docs.oracle.com/javase/8/docs/api/java/util/function/BiFunction.html)
|(T, U)|boolean|[BiPredicate<T,U>](https://docs.oracle.com/javase/8/docs/api/java/util/function/BiPredicate.html)
|(T, U)|int|[ToIntBiFunction<T,U>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ToIntBiFunction.html)
|(T, U)|long|[ToLongBiFunction<T,U>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ToLongBiFunction.html)
|(T, U)|double|[ToDoubleBiFunction<T,U>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ToDoubleBiFunction.html)
|(T, int)|void|[ObjIntConsumer<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ObjIntConsumer.html)
|(T, long)|void|[ObjLongConsumer<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ObjLongConsumer.html)
|(T, double)|void|[ObjDoubleConsumer<T>](https://docs.oracle.com/javase/8/docs/api/java/util/function/ObjDoubleConsumer.html)
|(int)|void|[IntConsumer](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntConsumer.html)
|(int)|R|[IntFunction<R>](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntFunction.html)
|(int)|boolean|[IntPredicate](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntPredicate.html)
|(int)|int|[IntUnaryOperator](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntUnaryOperator.html)
|(int)|long|[IntToLongFunction](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntToLongFunction.html)
|(int)|double|[IntToDoubleFunction](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntToDoubleFunction.html)
|(int, int)|int|[IntBinaryOperator](https://docs.oracle.com/javase/8/docs/api/java/util/function/IntBinaryOperator.html)
|(long)|void|[LongConsumer](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongConsumer.html)
|(long)|R|[LongFunction<R>](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongFunction.html)
|(long)|boolean|[LongPredicate](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongPredicate.html)
|(long)|int|[LongToIntFunction](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongToIntFunction.html)
|(long)|long|[LongUnaryOperator](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongUnaryOperator.html)
|(long)|double|[LongToDoubleFunction](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongToDoubleFunction.html)
|(long, long)|long|[LongBinaryOperator](https://docs.oracle.com/javase/8/docs/api/java/util/function/LongBinaryOperator.html)
|(double)|void|[DoubleConsumer](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoubleConsumer.html)
|(double)|R|[DoubleFunction<R>](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoubleFunction.html)
|(double)|boolean|[DoublePredicate](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoublePredicate.html)
|(double)|int|[DoubleToIntFunction](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoubleToIntFunction.html)
|(double)|long|[DoubleToLongFunction](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoubleToLongFunction.html)
|(double)|double|[DoubleUnaryOperator](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoubleUnaryOperator.html)
|(double, double)|double|[DoubleBinaryOperator](https://docs.oracle.com/javase/8/docs/api/java/util/function/DoubleBinaryOperator.html)

