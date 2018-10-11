# session.sh
# Use this file to track the commands that you execute in your terminal
# You don't need to *run* this file, just list out the appropriate command after each prompt

# Print your working directory
pwd


# Change your directory to a folder in which you do work for this class
cd /Users/samuelbuxton/Documents/INFO201/

# You may use "~" shortcut (mac), or specify the full path


# Clone your (private) assignment repository from GitHub to your machine
git clone https://github.com/info201b-s18/a1-news-sambuxton.git

# Change your directory to inside of your "a1-news-USERNAME" folder
cd a1-news-sambuxton #assuming I'm already in the "INFO 201" folder

# Make a new folder called "imgs" - you'll download an image into this folder
mkdir imgs


# At appropriate checkpoints, you'll need to do the following version control steps
# (feel free to only list these steps once)
git add README.md #adds file to staging area, assuming that this is the file we are saving
git commit -m "this is a commit message in the imperative mood" README.md #records changes to local repository with message
git push origin master #publishes file to online repository's master branch

