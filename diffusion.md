# FEM for diffusion equation

## Governing equation

$$
-\nabla^{2}u\left(\mathbf{x}\right)=f\left(\mathbf{x}\right),\text{ in }\Omega
$$

## Boundary conditions

### Dirichlet boundary condition

$$
u=g, \text{ on } \Gamma_{g}
$$

### Neumann boundary condition

$$
\frac{\partial u}{\partial n} = h, \text{ on } \Gamma_{h}
$$

Here, $h$ denotes the outgoing flux.

### Mixed/Generalized boundary condition

$$
\alpha u+\beta\frac{\partial u}{\partial n}=g\text{ on }\Gamma_{g}
$$

- The coefficients $\alpha$ and $\beta$ can be constant, or varying in space, or with solution
- If the coefficients depends upon the solution, then we obtain a nonlinear diffusion equation

## Pure Neumann problem

If at all boundary Neumann boundary condition is prescribed, then the solution to the problem can be unique up to an additive constant.

Also, by noting following identity,

$$
-\int\nabla\cdot\nabla u\Omega=-\int\nabla u\cdot\mathbf{n}ds=\int fd\Omega,
$$

we can derive the following necessary condition, which is also known as the compatibility condition, for the existence of a solution to a Neumann problem.

$$
\int_{\Gamma}gd\Omega+\int_{\Omega}fd\Omega=0
$$

## Example-1

- Homogeneous Dirichlet boundary condition
- $f(x)=1$
- Square shaped domain $\Omega := (-1,1)\times (-1,1)$

### Analytical solutions

$$
u\left(x,y\right)=\frac{1-x^{2}}{2}-\frac{16}{\pi^{3}}\sum_{k=1,odd}^{\infty}\left\{ \frac{1}{k^{3}\sinh(k\pi)}\sin\left(\frac{k\pi(1+x)}{2}\right)\times\left(\sinh\frac{k\pi(1+y)}{2}+\sinh\frac{k\pi(1-y)}{2}\right)\right\}
$$

## Example-2

- Homogeneous Dirichlet boundary condition
- $f(x)=1$
- Square shaped domain $\Omega := (-1,1)\times (-1,1)$ minus $(-1,0] \times (-1, 0]$

The solution $u$ is closely approximated at the origin by the function.

$$
u(r,\theta)=r^{2/3}\sin\left(\frac{2\theta+\pi}{3}\right)
$$

$\theta$ is the angle from vertical axis.

## Example-3

Consider the following manufactured solutions.

$$
u(x,y)=\frac{2(1+y)}{(3+x)^{2}+(1+y)^{2}}
$$

## Weak formulation

A sufficiently smooth funciton $u$ satisfying the Laplace equation and boundary condition is known as the classical solution to BVP. For Dirichlet boundary value problem, $u$ is a classical solution if it has continuous second order derivative in $\Omega$ and it is at-least continuous near and at the boundary.

> In cases of non-smooth domains or discontinuous source functions, the function $u$ satisfying the BVP may not be smooth enough to be regarded as a classical solution

In such cases, an alternative description of the BVP is required, which is less restrictive in terms of smoothness of the admissible solution. This alternative description of BVP is what we referred to as weak formulation.

To derive a weak formulation of a Poisson problem, we require that for an appropriate set of test functions, $\delta u$

$$
\int_{\Omega}\delta u\left(\nabla^{2}u+f\right)d\Omega=0
$$

Now, using integration by parts, we can derive the weak formulation, which states that find the solution $u$ in some functional space such that for all $\delta u$ in some functional space, following is holds true.

$$
\int_{\Omega}\nabla\delta u\cdot\nabla ud\Omega-\int_{\Omega}\delta ufd\Omega-\int_{\Gamma}\delta u\nabla u\cdot\mathbf{n}ds=0
$$

### L2 space

$$
L_{2}(\Omega):=\left\{ u:\Omega\rightarrow\mathbb{R}\vert\int_{\Omega}u^{2}d\Omega<\infty\right\}
$$

### Sobolev space

$$
\mathcal{H}^{1}(\Omega):=\left\{ u:\Omega\rightarrow\mathbb{R}\vert u,\frac{\partial u}{\partial x_{i}}\in L_{2}\left(\Omega\right),i=1,2,3\right\}
$$

Now we can define the functional space for test function and solution.

$$
\mathbb{V}:=\left\{ u\in\mathcal{H}^{1}\left(\Omega\right)\vert u=0,\text{on}\Gamma_{g}\right\}
$$

$$
\mathbb{S}:=\left\{ u\in\mathcal{H}^{1}\left(\Omega\right)\vert u=g,\text{on}\Gamma_{g}\right\}
$$

## Galerkin Finite Element Method

It is now time to present the idea of approximating $u$ by taking finite dimensional subspace of the solution space $\mathbb{S}$. We will denote the finite dimensional space by $\mathbb{S}^{h}$

$$
a(u,v)=\int_{\Omega}{\nabla {u} \cdot \nabla {v}}{d\Omega}
$$

$$
l(v):= \int_{\Omega}{vf}d\Omega + \int_{\Gamma_{h}}{vh}dS
$$

Weak form

Find $u \in \mathcal{H}^{1}_{E}$ such that

$$
a(u,v)=l(v), \text{ for all } v\in \mathcal{H}^{1}_{E0}
$$

The corresponding discrete problem is given by

Find $u_{h} \in \mathbb{S}^{h} \subset \mathcal{H}^{1}_{E}$ such that

$$
a(u_{h},v_{h})=l(v_{h}), \text{ for all } v_{h} \in \mathbb{S}_{0}^{h} \subset \mathcal{H}_{E0}^{1}
$$

## Best approximation property

$$
a(u,v)=l(v),\forall v\in\mathcal{H}_{0}^{1}
$$

$$
\begin{aligned}a\left(u,v\right)-a(u_{h},v) & =l(v)-a(u_{h},v),\forall v\in\mathcal{H}_{0}^{1},\forall u_{h}\in\mathbb{S}^{h}\subset\mathcal{H}^{1}\\
a(u-u_{h},v) & =l(v)-a(u_{h},v),\forall v\in\mathcal{H}_{0}^{1},\forall u_{h}\in\mathbb{S}^{h}\subset\mathcal{H}^{1}\\
a(e,v) & =l(v)-a(u_{h},v),\forall v\in\mathcal{H}_{0}^{1},\forall u_{h}\in\mathbb{S}^{h}\subset\mathcal{H}^{1}
\end{aligned}
$$

Note that $e:=u-u_h \in \mathcal{H}_{0}^{1}$.

Using Galerkin Orthogonality Property we can shown that

$$
\left\Vert \nabla\left(u-u_{h}\right)\right\Vert \le\left\Vert \nabla\left(u-v_{h}\right)\right\Vert ,\forall v_{h}\in\mathbb{S}^{h}
$$

By using Poincare-Friedrichs-Inequality and Trace-Inequality we can also show that

$$
\Vert u-u_{h}\Vert_{H^{1}}\le C_{\Omega}\min_{v_{h}\in\mathbb{S}^{h}}\left(\Vert u-u_{h}\Vert_{H^{1}}\right)
$$