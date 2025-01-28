# Approximate solution

Consider the following elliptic differential equation

$$
-\frac{d}{dx}\left(\nu\frac{du}{dx}\right)+ku-f(x)=0,\text{ on }x\in\left[0,L\right]
$$

where, $\nu > 0$  and $k\ge 0$.  In this equation u can be a scalar or a vector. We will consider homogeneous essential boundary condition for simplicity, that is,

$$
u(0)=u(L)=0
$$

Let us approximate the solution by following expression.

$$
u_{N}(x)=\sum_{i=1}^{N}a_{i}\phi_{i}\left(x\right)
$$

where, $a_{i}$ are coefficients which we want to find, and $\phi_{i}$ is the basis function:

$$
\phi_{i}(x)=x^{i}(L-x)
$$

not that, this function satisfy $\phi_{i}(0)=\phi_{i}(L)=0$, therefore, $u_{N}(x)$ also satisfy the boundary condition.

The error is defined by

$$
e_{N}(x)=u(x)-u_N(x)=u(x)-\sum_{i=1}^{N}a_{i}\phi_{i}\left(x\right)
$$

Consider the following functional for error

$$
\mathcal{I}:=\frac{1}{2}\int_{0}^{L}\nu\left(\frac{de_{N}}{dx}\right)^{2}dx+\frac{1}{2}\int_{0}^{L}ke_{N}^{2}dx
$$

We determine $a_{i}$ by minimizing the above functional, that is,

$$
\frac{\partial\mathcal{I}}{\partial a_{i}}=0
$$

This will yield $N$ equation of the following form:

$$
\int_{0}^{L}\nu\frac{de_{N}}{dx}\frac{d}{da_{i}}\left(\frac{de_{N}}{dx}\right)dx+\int_{0}^{L}ke_{N}\frac{de_{N}}{da_{i}}dx=0
$$

Noting that

$$
\begin{aligned}\frac{\partial e_{N}(x)}{\partial a_{i}} & =\frac{\partial\left(u(x)-\sum_{j=1}^{N}a_{j}\phi_{j}\left(x\right)\right)}{\partial a_{i}}\\ & =-\sum_{j=1}^{N}\frac{\partial a_{j}}{\partial a_{i}}\phi_{j}\left(x\right)\\ & =-\phi_{i}\left(x\right) \end{aligned}
$$

we get:

$$
\int_{0}^{L}\nu\frac{de_{N}}{dx}\frac{d\phi_{i}}{dx}dx+\int_{0}^{L}ke_{N}\phi_{i}dx=0
$$

$$
\begin{aligned}\int_{0}^{L}\nu\frac{du_{N}}{dx}\frac{d\phi_{i}}{dx}dx+\int_{0}^{L}ku_{N}\phi_{i}dx & =\int_{0}^{L}\left(\nu\frac{du}{dx}\frac{d\phi_{i}}{dx}+ku\phi_{i}\right)dx\\ & =\int_{0}^{L}\left(-\nu\frac{d^{2}u}{dx^{2}}\phi_{i}+ku\phi_{i}\right)dx \end{aligned}
$$
In the second step we have employed integration by parts and the fact that $\phi_{i}$ vanishes at the boundary. Finally we can obtain the following system of linear equations.

$$
\int_{0}^{L}\nu\frac{du_{N}}{dx}\phi_{i}dx+\int_{0}^{L}ku_{N}\phi_{i}dx=\int_{0}^{L}f(x)\phi_{i}dx, \text{ for } i=1,N
$$

In matrix form

$$
\mathbf{K}\mathbf{a}=\mathbf{F}
$$

where $\mathbf{a}=\left\{ a_1, a_2, \cdots, a_{N}\right \}$ and

$$
K_{ij}=\int_{0}^{L}\nu\frac{d\phi_{j}}{dx}\frac{d\phi_{i}}{dx}dx+\int_{0}^{L}k\phi_{j}\phi_{i}dx
$$

$$
F_{i}=\int_{0}^{L}f(x)\phi_{i}dx
$$

## Basis function

$\phi_{i}$ are called the basis function, which means these N basis functions are linearly independent. That is, if

$$
\sum_{i=1}^{N}a_j \phi_{j}=0
$$
then, $a_j=0,\forall j=1,2,\cdots,N$ . Because the basis functions are independent the matrix $\mathbf{K}$ is non-singular and invertible.   These span of these N basis functions forms a function space given by

$$
S:=span\left\{\phi_{i}\right \}:=\left \{ u_{N} \vert u_N=\sum_{i=1}^{N}a_j \phi_{j} \right \}
$$
