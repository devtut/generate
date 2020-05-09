---
metaTitle: "MATLAB - Matrix decompositions"
description: "Cholesky decomposition, QR decomposition, LU decomposition, Schur decomposition, Singular value decomposition"
---

# Matrix decompositions



## Cholesky decomposition


The Cholesky decomposition is a method to decompose an hermitean, positiv definite matrix into an upper triangular matrix and its transpose. It can be used to solve linear equations systems and and is around twice as fast as LU-decomposition.

```matlab
A = [4 12 -16
    12 37 -43
    -16 -43 98];
R = chol(A);

```

This returns the upper triangular matrix. The lower one is obtained by transposition.

```matlab
L = R';

```

We finally can check whether the decomposition was correct.

```matlab
b = (A == L*R);

```



## QR decomposition


This method will decompose a matrix into an upper triangular and an orthogonal matrix.

```matlab
A = [4 12 -16
    12 37 -43
    -16 -43 98];
R = qr(A);

```

This will return the upper triangular matrix while the following will return both matrices.

```matlab
[Q,R] = qr(A);

```

The following plot will display the runtime of `qr` dependent of the square root of elements of the matrix.
[<img src="http://i.stack.imgur.com/yXM8a.jpg" alt="qr-runtime" />](http://i.stack.imgur.com/yXM8a.jpg)



## LU decomposition


Hereby a matrix will be decomposed into an upper trangular and an lower triangular matrix. Often it will be used to increase the performance and stability (if it's done with permutation) of Gauß elimination.

However, quite often does this method not or badly work as it is not stable. For example

```matlab
A = [8 1 6
    3 5 7
    4 9 2];
[L,U] = lu(A);

```

It is sufficient to add an permutation matrix such that PA=LU:

```matlab
[L,U,P]=lu(A);

```

In the following we will now plot the runtime of `lu' dependent of the square root of elements of the matrix.
[<img src="http://i.stack.imgur.com/cnhNK.jpg" alt="lu-runtime" />](http://i.stack.imgur.com/cnhNK.jpg)



## Schur decomposition


If A is a complex and quadratic matrix there exists a unitary Q such that Q*AQ = T = D + N with D being the diagonal matrix consisting of the eigenvalues and N being strictly upper tridiagonal.

```matlab
A = [3 6 1
    23 13 1
    0 3 4];
T = schur(A);

```

We also display the runtime of `schur` dependent on the square root of matrix elements:
[<img src="http://i.stack.imgur.com/1wnKd.jpg" alt="schur-runtime" />](http://i.stack.imgur.com/1wnKd.jpg)



## Singular value decomposition


Given an m times n matrix A with n larger than m. The singular value decomposition

```matlab
[U,S,V] = svd(A);

```

computes the matrices U,S,V.

The matrix U consists of the left singular eigenvectors which are the eigenvectors of `A*A.'` while V consists of the right singular eigenvalues which are the eigenvectors of `A.'*A`. The matrix `S` has the square roots of the eigenvalues of `A*A.'` and `A.'*A` on its diagonal.

If m is larger than n one can use

```matlab
[U,S,V] = svd(A,'econ');

```

to perform economy sized singular value decomposition.



#### Syntax


1. R = chol(A);
1. [L,U] = lu(A);
1. R = qr(A);
1. T = schur(A);
1. [U,S,V] = svd(A);

