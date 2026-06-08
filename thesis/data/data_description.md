# Data Description

## 1. Gait data:
- Source: 
- Number of Observations:
- Variables:
- Description:

## 2. NBA data:
- Source:
- Number of Observations:
- Variables:
- Description:

## 3. Simulation Data:
### Purpose
The simulation study was conducted to evaluate the performance of the proposed t-SNE visualization method for trajectory data. The method was compared with three alternatives: traditional t-SNE using Euclidean distance, t-SNE using Fréchet distance and t-SNE using Dynamic Time Warping (DTW).

### Experimental Design
We considered three groups, each generated from different probability density functions, $\phi_X(t)$. Each group contains an equal number of observations, where each observation represents a trajectory.
- Number of observations: $n \in$ {90, 180, 270}
- Trajectory length: $l \in$ {50, 100}
- Number of groups: 3
  
### Data Generating Process
Each trajectory is generated based on covariates $X$ and $Y$, where:

**Group 1**
- $X \sim N(4 + U, 1)$
- $Y \sim N(35, 2^2)$

**Group 2**
- $X \sim N(7 + U, 1)$
- $Y \sim N(45, 2^2)$

**Group 3**
- $X \sim 0.4 \times N(3 + U, 1) + 0.6 \times N(7 + U, 1)$
- $U \sim U(-1, 1)$
- $Y \sim N(40, 2^2)$

### Missing Data Mechanism
To assess robustness under incomplete observations, missingness was introduced as follows:
- Missing rate: $\phi \in$ {30%, 50%}
- Missing time points were randomly selected across trajectories

### Evaluation Procedure
The performance of visualization methods was evaluated using:
- Wilk's lambda
- Pillai's trace
- Hotelling-Lawley trace
- Classification accuracy

Lower values of Wilk's lambda and higher values of the other metrics indicate better performance.

Accuracy was computed using:
- Logistic regression (for two groups)
- K-means clusterinf (for more than two groups)

Each experiment was repeated 50 times, and the mean and standard deviation of the evaluation metrics were reported.

### Output
- Results (evaluation metrics and visualizations) stored in `/results/simulation`
