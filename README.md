# Replication code for "Adding to the Debate: Does Immediate or Delayed Feedback Make a Difference?"

## Instructions for Replication:

### Accessing the Data

To access the data, follow instructions at [https://osf.io/r3nf2/](https://osf.io/r3nf2/),  download the following data files, and place them in the [data/](data/) subfolder:

- assess_student.csv
- student_demo.csv
- student_roster.csv
- assist_student_problem.csv

### Replication in R:

To replicate in `R`, make sure that the packages listed in [sessionInfo.txt](results/sessionInfo.txt) and run:
```
source('code/feedbackOverall.r')
```

A record of our exploration of alternative models can be found in [code/exploration/](code/exploration/).
