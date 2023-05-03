# VTEC Workshop Module 4 Lab

The lab assignment is written in Rmarkdown (.Rmd) which means that the lab is intended to be run in RStudio IDE.

## Set up

Before connecting to your RStudio instance, please perform the following steps:

1. Log into your remote instance using ssh

```bash
# replace xx with your student number
ssh -i CBW.pem xx.uhn-hpc.ca
```

2. Copy the lab materials from `CourseData` to `workspace/`

```bash
cp -r  ~/CourseData/IDE_data/module4/ ~/workspace/
```

3. You should now find a directory called `module4` under `~/workspace/`. Use `ls` to list the file content in `~/workspace/module4` to verify that you have successfully copied all of the required files.

```bash
ls ~/workspace/module4
```

Expected output:

<img src="https://raw.githubusercontent.com/jimmyliu1326/vtec23_integrated/main/img/ls.png" width="750"/>

## Launching RStudio

1. Open a browser tab and navigate to `http://xx.uhn-hpc.ca:8080` (Remember to replace `xx` with your student number!)

2. Log in using the following credentials:
    - Username: `ubuntu`
    - Password will be provided in class

    If logged in successfully, you should see the following graphical interface in your browser:

<img src="https://raw.githubusercontent.com/jimmyliu1326/vtec23_integrated/main/img/rstudio.png" width="750"/>

3. Load up `cgMLST_workflow.Rmd` by clicking [File] -> [Open File] -> [Select the .Rmd file in `module4` directory]

    If you have successfully opened the `.Rmd` file, you will see the following screen:

<img src="https://raw.githubusercontent.com/bioinformaticsdotca/IDE_2023/main/module5/img/rmarkdown.png" width="750"/>