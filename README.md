# NLP-EIT-DSC-IS-project

## How to run it? 
1. Download the script "assignment.R" and the dataset "news.csv" from this repo. 
2. Locate the script and the folder in the directory that you want on your computer but they MUST BE together in the same directory. 
3. Open the file "assignment.R" with RStudio program.
4. In R Studio there are two ways to running scripts: 
  - a) You can run it line by line just selecting the line and clicking "run" but ensure to run every line above the comment "MAIN: START HERE" in the script. 
    Then the application can be run starting in line "message("Welcome to newsSummary App what can I do for you? ")". 
  - b) The recommended way to run the app is just clicking the checkbox "Source on Save" and by saving all the script is launched without the need of executing      the above as in option a)
 
 5. When the application is launched the first things it ask to you is to provide the complete path of the folder where is located the script "assignment.R"      and the dataset "news.csv". IMPORTANT: To reduce the time in execution use the dataset "news_short.csv" provided also in this repo. 
 6. After selecting the right path for the folder and the dataset the application gives you five options: 
  - a) Running an option that would answer your questions regarding the text in "content" column in the dataset. 
  - b) Running an option to summarize a new that you can select. The way to run it is type "summary". 
    The summary contains all the names, locations, dates and organizations mentioned in the new.
  - c) Say hello to the application that will answer you in the same way. 
  - d) Say goodbye to the application that will say goodbye to you also. 
  - e) Quit from the application just typing in the prompt the word "quit". 

## IMPORTANT: 
The dataset is provided in a small version in this repo ("news_short.csv") since the original dataset is bigger than the size supported for uploading files in Github. However, the original dataset can be downloaded from the following link: https://www.kaggle.com/snapcrack/all-the-news and there download any file provided there.
  
