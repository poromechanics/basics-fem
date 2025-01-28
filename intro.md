# Introduction

- In recent times, many engineering decisions are based on numerical simulations with the expectation that the simulations will provide a reliable quantitative estimate of some attributes of a physical system or process.
- Numerical simulations take *less time and money*. Therefore, “**Numerical Simulation Driven Design Processes**” has gained significant attraction in many industries such as civil, aerospace, automative, biomedical, defense, energy, electronics, heavy industry

## Conceptualization and Mathematical models

Conceptualization is a process through which we idealize an engineering problem in the physical reality. The error induced due to the conceptualization is called the error due to the idealization.

![The Flow of Numerical Simulations](./figures/numerical-simulation-flow.svg)

The goal of conceptualization is to identify the simplest mathematical model that can provide predictions of the data of interest within a specified range of accuracy. In other words, the error due to the idealization should be as small as possible.

A numerical simulation requires an underlying mathematical model. Mathematical models are mathematical description of the idealized problem, which we have obtained through the process of conceptualization. In mechanics, these mathematical models are described by the sets of differential equations and boundary and initial conditions. These differential equations are collectively called as the *governing equations* of the problem. 

:::{.callout-note appearance="simple"}
It is important to note that the mathematical models are the idealized representations of the reality, and we should never be confused with the physical reality that these models are supposed to represent.
:::

Further, the development of a mathematical model is not a trivial task, and following questions often help us in preparing the mathematical model.

- What aspects, features or phenomena of physical reality are of interest?
- What data must be predicted?
- What accuracy is required?

For example, in solid mechanics, we often estimates **displacements**, **stresses**, **strains**, **natural frequencies**, etc., for a given computation domain, stress-strain model, and boundary conditions. The mathematical model is given by a set of partial differential equations (PDEs), which are obtained by satisfying:

- conservation of linear and angular momentum
- conservation of mass
- conservation of energy

The mathematical model obtained by satisfying the above conservation principals will yield a general description of the problem in the mechanics. These governing equations are valid for solids, liquids, and gases. However, in my opinion these models are computationally useless unless we make valid assumptions to simplify them to an extent so that they are computationally feasible. These assumptions are made while considering the material behavior, mode of deformation, dimensional reduction, etc.  

For example, the usual beam, plate and shell model (theory) are special cases of a model based on the three-dimensional linear theory of elasticity, which in turn is a special case of a large family of models based on the equations of continuum mechanics.  This is what we called the *Hierarchic view of mathematical models*.

:::{.callout-note title="Example"}
In practice we often starts from a very simple model, and then add challenging features in the model to capture the reality as accurately as possible.
:::

## Validation

At this point we know that a mathematical model represents the idealization of an engineering problem. Therefore, every mathematical model has some error due to this idealization (or conceptualization). Validation is a process by which the numerical results of a mathematical model are tested against the experimental data. Through validation process we can estimate the error of conceptualization and we can have an idea about the predictive capabilities of the model.

If the error induced by the model is smaller than tolerance limit, then we can say that the model has passed the validation test. Otherwise, we need to improve the mathematical model until it passes the validation test.

Depending upon the problem, such as design of an aircraft or design of a concrete dam, a series of validation experiments are performed. These validation experiments include the determination of physical properties and failure criteria for entire structure, sub-components, parts, etc. 

While assessing the results of validation experiments it is important to consider the limitations and uncertainties associated with the available information concerning the physical system being modeled.

- The computation domain is usually assumed to corresponds to design specification. In reality, parts, sub-components and assemblies can deviate from their specifications and the degree of deviation may not be know, or would be difficult to incorporate into a model. For example, a concrete dam or earthen dam has many components, joints and heterogeneity, which may be too complex to include in a mathematical model.
- The exact spatial-temporal distribution of the material properties may not be known accurately. Moreover, for many materials the constitutive laws are not known accurately.
- The boundary conditions may not be known with a high degree of precision.
- The magnitude of residual stresses inside the material may be not be known.

## Discretization

Once the mathematical model is ready, we use numerical methods to solve the problem by using computers. The finite element method (FEM) is one of the most powerful and widely used numerical methods for finding approximate solutions to mathematical models. In FEM, the computation domain is divided into several elements of simple shapes such as triangle, quadrangle, tetrahedron, hexahedron, among others.  

## Verification

In the verification process we verify that

- the input data are correct
- the computer code is functioning properly
- the numerical errors in the computed solution is lower than the accepted limit
