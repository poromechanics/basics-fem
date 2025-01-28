# Generalized formulation in one dimension

Once again consider the following differential equation:

$$
-\frac{d}{dx}\left(\nu\frac{du}{dx}\right)+ku-f(x)=0,\text{ on }x\in\left[0,L\right]
$$

Let $\delta u$ be a function (belonging to some functional space) defined on the line interval $\left[0,L\right]$, then we focus on following integral form (variational form)

$$
\int_{0}^{L}\delta u\left(-\frac{d}{dx}\left(\nu\frac{du}{dx}\right)+ku-f(x)\right)dx=0
$$

noting that

$$
\delta u\frac{d}{dx}\left(\nu\frac{du}{dx}\right)=\frac{d}{dx}\left(\delta u\nu\frac{du}{dx}\right)-\frac{d\delta u}{dx}\nu\frac{du}{dx}
$$

we obtain the following

$$
\begin{split}\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx\\ =\int_{0}^{L}\delta uf(x)dx+\int_{0}^{L}\frac{d}{dx}\left(\delta u\nu\frac{du}{dx}\right)dx \end{split}
$$

$$
\begin{split}\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx\\ =\int_{0}^{L}\delta uf(x)dx+\left(\delta u\nu\frac{du}{dx}\right)_{x=L}-\left(\delta u\nu\frac{du}{dx}\right)_{x=0} \end{split}
$$

## Functional space

Now the big question is where does $\delta u$ and $u$ belong? To get this answer, in the above variational form, let us put $\delta u=u$ and assume that $u$ vanishes at the boundary:

$$
\int_{0}^{L}\frac{d u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L} ukudx=\int_{0}^{L} f(x)dx
$$

Now we can see that both $u,\frac{d u}{dx}$ should be finite in order to get finite value of integrals on the left hand side. Moreover, the left hand side integral represents the variation of the potential energy of the system which is denoted by $U(u)$:

$$
U(u):=\frac{1}{2}\int_{0}^{L}\left ( \nu(\frac{du}{dx})^2+ku^2 \right )dx
$$

Now we are looking for those function which has finite value of $U$, we call the space of these functions as energy space, denoted by $E([0,L])$:

$$
E([0,L]):=\left \{ u \vert U(u) < \infty \right \}
$$

Now we want $u$ to satisfy the boundary conditions $u(0)=u(L)=0$, however, not all functions in $E([0,L])$ will fulfill this condition. Therefore, we are looking for a subspace of $E([0,L])$, denoted here by $E^{0}([0,L])$ such that

$$
E^0([0,L]):=\left \{ u \vert u \in E([0,L]), u(0)=u(L)=0 \right \}
$$

## Essential boundary condition

To proceed further we will assume that $u \in \tilde{E}[0,L]$ and $\delta u \in E^{0}[0,L]$, where both these spaces are subspaces of energy-space. The exact definition of these two spaces will depend upon the type of essential boundary condition as discussed below.

**Case-1:** When $u(0)=u(L)=0$, then

$$
\tilde{E}[0,L]=E^0([0,L]):=\left \{ u \vert u \in E([0,L]), u(0)=u(L)=0 \right \},
$$

The variation form is given by

$$
\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx=\int_{0}^{L}\delta uf(x)dx
$$

**Case-2:** When $u(0)=g_0, u(L)=g_L$, then

$$
\tilde{E}([0,L]):=\left \{ u \vert u \in E([0,L]), u(0)=g_{0},u(L)=g_{L} \right \},
$$

and $E^0[0,L]$ is same as the one defined in Case-1. The variational form is same as the Case-1.  

**Case-3**: When $u(0)=g_{0}$, then 

$$
\tilde{E}([0,L]):=\left \{ u \vert u \in E([0,L]), u(0)=g_{0} \right \},
$$

$$
{E}^{0}([0,L]):=\left \{ u \vert u \in E([0,L]), u(0)=0 \right \},
$$

The variation form is given by:

$$
\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx=\int_{0}^{L}\delta uf(x)dx+\left(\delta u\nu\frac{du}{dx}\right)_{x=L}
$$

**Case-4**: When $u(L)=g_{L}$, then 

$$
\tilde{E}([0,L]):=\left \{ u \vert u \in E([0,L]), u(L)=g_{L} \right \},
$$

$$
{E}^{0}([0,L]):=\left \{ u \vert u \in E([0,L]), u(L)=0 \right \},
$$

The variation form is given by:

$$
\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx=\int_{0}^{L}\delta uf(x)dx-\left(\delta u\nu\frac{du}{dx}\right)_{x=0}
$$

In is evident from the above discussion that the choice of spaces for test and trial functions depends upon the type of essential boundary conditions. It is worth noting that the test function $\delta u$ vanishes at those points where $u$ is prescribed.

There is way to deal with the essential boundary condition. In this method we write the solution as follows

$$
u(x) = u_f+u_p
$$

where, $u_{f} \in E^{0}[0,L]$, that is, it vanishes at the boundary. We can select $u_p$ such that $u$ satisfy the boundary condition. Usually, $u_p$ depends upon the boundary value of $u$.  Now the variational form reads:

$$
\begin{split}\int_{0}^{L}\frac{d\delta u_{f}}{dx}\nu\frac{du_{f}}{dx}dx+\int_{0}^{L}\delta u_{f}ku_{f}dx & =\int_{0}^{L}\delta u_{f}f(x)dx\\ & -\int_{0}^{L}\frac{d\delta u_{f}}{dx}\nu\frac{du_{p}}{dx}dx\\ & -\int_{0}^{L}\delta u_{f}ku_{p}dx \end{split}
$$

In this case $\delta u_{f}, u_f \in E^{0}[0,L]$.

## Neumann boundary condition

When we specify the value of $\nu \frac{du}{dx}$ on the boundary, then these type of boundary conditions are called Neumann boundary condition or natural boundary condition. 

Note that we do not prescribe Essential boundary condition and Neumann boundary condition for a given component on the same boundary of the computation domain.

Case-1: We prescribe Neumann boundary on both ends.

$$
\nu \frac{du}{dx}\vert_{x=0} = F_0
$$

$$
\nu \frac{du}{dx}\vert_{x=L} = F_L
$$

Then the variational form reads:

$$
\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx=\int_{0}^{L}\delta uf(x)dx+\delta u(L) F_L-\delta u(0) F_0
$$

Both $\delta u, u \in E[0,L]$, that is $\tilde{E}[0,L]=E^{0}[0,L]=E[0,L]$. 

:::{.callout-note appearance="simple"}
Because we have not prescribed essential boundary conditions there is no need for $\delta u$ to vanish on the boundary.
:::

## Robin boundary condition

Robin boundary condition is a combination of essential and Neumann boundary in the sense that:

$$
a u + b \frac{du}{dx} = c, \text{ on boundary}
$$

where, $a,b,c$ are constants. 

We consider the robin boundary condition of the following form:

$$
\nu \frac{du}{dx}\vert_{x=0} = \beta_{0}(u(0)-U_0)
$$

$$
\nu \frac{du}{dx}\vert_{x=L} = \beta_{L}(U_L-u(L))
$$

where, $\beta_{0}, \beta_{L}, U_0, U_L$ are constants.

The variation form is given by:

$$
\begin{split}\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx\\ +\delta u(0)\beta_{0}u(0)+\delta u(L)\beta_{L}u(L)=\\ \int_{0}^{L}\delta uf(x)dx+\delta u(L)\beta_{L}U_{L}-\delta u(0)\beta_{0}U_{0} \end{split}
$$

## Rayleigh-Ritz method

Consider the following problem:

$$
-\frac{d}{dx}\left(\nu\frac{du}{dx}\right)+ku-f(x)=0,\text{ on }x\in\left[0,L\right]
$$

with homogeneous Dirichlet boundary conditions $u(0)=u(L)=0$.  

Then Rayleigh-Ritz method approximate the solution by

$$
u\approx\sum_{I=1}^{N}\phi_{I}a_{I},
$$

where, $\phi_{I}$ are the independent basis functions, and $a_{I}$ are the arbitrary constants. It is important to note that $\phi_{I}$ satisfies the boundary condition, therefore, $u$ satisfy the boundary condition. Now, we substitute this approximation in the the following variation form.

$$
\int_{0}^{L}\frac{d\delta u}{dx}\nu\frac{du}{dx}dx+\int_{0}^{L}\delta ukudx =\int_{0}^{L}\delta uf(x)dx
$$

$$
\left[\int_{0}^{L}\frac{d\phi_{I}}{dx}\nu\frac{d\phi_{J}}{dx}dx\right]a_{J}+\left[\int_{0}^{L}\phi_{I}k\phi_{J}dx\right]a_{J}=\int_{0}^{L}\phi_{I}f(x)dx
$$

This can be written in the following matrix-vector form:

$$
\left[{\bf K}\right]\left\{ {\bf A}\right\} =\left\{ {\bf F}\right\}
$$

### Example of basis function

$$
\phi_{I} = x^{I}(L-x)
$$

## Exercise

- See Exercise 2.3.2 to 2.3.4 of Szabo and Babuska (2011).
