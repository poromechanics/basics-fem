# Conceptualization

Consider an engineering problem of designing the foundation of a high-raise building. The pile transfers some of the load due to the building to the surrounding soils through shearing process. In addition, the pile transfer some of the loads to the soils at the greater depth. The building is resting on the piles, therefore, the engineer should ensure that the pile should not experience the excessive settlements. In this context, it is important to ensure that the pile has sufficient stiffness so that the elastic deformations in the piles are in permissible limits.

![Soil pile interaction](../figures/soil-pile-system.svg){#fig-soil-pile-system}

The mathematical model for the pile can be given by

$$
-AE\frac{d^{2}u}{dy^{2}}+k_{s}(y)u=0
$$

where, 

- $u(y)$, is the vertical displacement of the pile at $y=0$. 
- $A$ is the cross section area of the pile
- $E$ is the Young's modulus of the pile
- $k_{s}$ is represents the action of the soil on the pile.

Also, note that the downward direction is taken as positive. 

Under compressible forces following boundary conditions can be consider to complete the model:

$$
AE\frac{du}{dy}=F_{0},\quad \text{at } y=0
$$

$$
u(L)=0
$$

Under tensile forces following boundary conditions can be considered.

$$
AE\frac{du}{dy}=F_{0},\quad \text{at } y=0
$$

$$
\frac{du}{dy}=0,\quad \text{at } y=L
$$
Our goal is to predict the displacement of the pile at $y=0$, when the pile  is subjected to tensile forces (pull-out).

## Model A

In model A we assume that $k_{s}$ is constant. Then the solution of the problem is given by following.

$$
u(y)=\frac{F_{0}}{AE\lambda}\frac{e^{\lambda y}+e^{\lambda\left(2L-y\right)}}{1-e^{2\lambda L}}
$$

and,

$$
\frac{du}{dy}=\frac{F_{0}\lambda}{AE\lambda(1-e^{2\lambda L})}\left(e^{\lambda y}-e^{\lambda\left(2L-y\right)}\right)
$$

also, 

$$
u_0=-u(0)=-\frac{F_{0}}{AE\lambda}\frac{1+e^{\lambda2L}}{1-e^{2\lambda L}}
$$

## Calibration of model A

At this point we do not know the value of $k_s$, or $\lambda=k_{s}/AE$. In order to find this value, we conduct the pull out test and find the relation between $F_0$ and $u_0$. Let us denote $E_{0}=F_{0}/u_0$.

Then, 

$$
\frac{F_{0}}{u_{0}}=-AE\lambda\frac{1-e^{\lambda2L}}{1+e^{2\lambda L}}
$$

or,

$$
f(\lambda):=E_{0}(1+e^{2\lambda L})+AE\lambda(1-e^{2\lambda L})=0
$$

This is a nonlinear equation in $\lambda$, which can be find by using the Newton-Raphson method. In this way we find the value of $\lambda$ and $k_{s}$, use the model to predict the response of pile under tension. See [[#Appendix]].
 

## Model B

In model B, it is assumed that $k_{s}=ky$. Then the governing equation becomes:

$$
-AE\frac{d^{2}u}{dy^{2}}+kyu=0
$$

or,

$$
-\frac{d^{2}u}{dy^{2}}+\lambda^2 yu=0
$$

where,

$$
\lambda = \sqrt{\frac{k}{AE}}
$$

The boundary conditions are

$$
\frac{du}{dy}\vert_{y=0}=F_{0}/AE
$$

$$
\frac{du}{dy}\vert_{y=L}=0
$$

## Calibration of model B

Noting that

$$
\frac{d^{2}u}{dy^{2}}=\lambda^{2}yu
$$

$$
\frac{d^{3}u}{dy^{3}}=\lambda^{2}u+\lambda^{2}y\frac{du}{dy}
$$

$$
\frac{d^{4}u}{dy^{4}}=2\lambda^{2}\frac{du}{dy}+\lambda^{2}y\frac{d^{2}u}{dy^{2}}
$$

$$
\frac{d^{5}u}{dy^{5}}=3\lambda^{2}\frac{d^{2}u}{dy^{2}}+\lambda^{2}y\frac{d^{3}u}{dy^{3}}
$$

Then,

$$
\frac{d^{k}u}{dy^{k}}=(k-2)\lambda^{2}\frac{d^{k-3}u}{dy^{k-3}}+\lambda^{2}y\frac{d^{k-2}u}{dy^{k-2}}
$$

The Taylor series expansion of $u(x)$ is given by

$$
u(y) = -u_0 + \frac{F_0}{AE}y - \frac{\lambda^2}{3!}u_0 y^3 + \frac{2\lambda^2}{4!} \frac{F_0}{AE}y^4-\frac{4\lambda^4}{6!} u_0 y^6+\cdots
$$

Now by using $\frac{du}{dy}\vert_{y=L}=0$ and the experimentally measured value of $F_{0}/u_{0}$, we can obtain $\lambda$ by solving following nonlinear equation

$$
\frac{F_0}{AE} - \frac{\lambda^2}{2}u_0 L^2 + \frac{2\lambda^2}{3!} \frac{F_0}{AE}L^3-\frac{4\lambda^4}{5!} u_0 L^5+\cdots=0
$$

## Validation of model A and model B

- [ ] TODO Add validation of model A and model B.


## Appendix

The following code calibrate the model A. Material properties are taken from Szabo and Babuska (2011), Chapter 2.

```fortran
program main
  implicit none

  real, parameter :: E0 = 22.22E+6
  real, parameter :: E = 200.0E+9
  real, parameter :: A = 1.402E-2
  real, parameter :: L = 12.0
  real, parameter :: resTol = 1.0E-6
  real :: f, df, lam, dlam
  integer, parameter :: maxIter = 100
  integer :: ii
  logical :: isconvg
  !!
  !! initial guess
  !!
  lam = 1.0; dlam = 0.0; isconvg = .FALSE.
  !!
  do ii = 1, maxIter
    f =   E0 &
      & + E0 * EXP( 2.0 * L * lam ) &
      & + A * E * lam &
      & - A * E * lam * EXP( 2.0 * L * lam )
    !!
    !! check convergence
    !!
    if( abs( f ) .LE. resTol ) then
      write( *, "(a)" ) "Convergence achieved"
      isconvg = .TRUE.
      exit
    else
      write( *, "(a, I6)" ) "Iteration = ", ii
      write( *, "(a, F16.6)" ) "residual = ", f
      write( *, "(a, F15.6)" ) "lam = ", lam
      write( *, * ) ""
      write( *, * ) ""
    end if
    !!
    df = &
      & 2.0 * E0 * L * EXP( 2.0 * L * lam ) &
      & + A * E &
      & - A * E * EXP( 2.0 * L * lam ) &
      & - 2.0 * A * E * L * lam * EXP( 2.0 * L * lam )
    !!
    !! calculate dlam
    !!
    dlam = -f / df
    !!
    !! update lam
    !!
    lam = lam + dlam
    !!
  end do
  !!
  !! print results
  !!
  if( isconvg ) then
    write( *, "(a, F12.6)" ) "lambda = ", lam
    write( *, "(a, F12.6)" ) "ks = ", lam*lam*A*E
  else
    write( *, "(a)" ) "No convergence, may be increase maxIter"
    write( *, "(a, F12.6)" ) "lambda = ", lam
    write( *, "(a, F12.6)" ) "ks = ", lam*lam*A*E
  end if
  !!
end program main
```