#Source: https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
#install.packages("plotly")
library(plotly)

# Choose application
oauth_endpoints("github")
myapp <- oauth_app(appname = "Harry_O_Brien_CSU33012",
                   key = "de353dc00fea8d0abbca",
                   secret = "c0772a26af1c1ce40943339de098c740ec1bd047")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- httr::config(token = github_token)
req <- GET("https://api.github.com/users/harryob2/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "harryob2/Biography-Assignment", "created_at"] 

################################################################################

### Collecting & Displaying My Data
# Retrieve data
myData = fromJSON("https://api.github.com/users/harryob2")

# Count followers
myData$followers

# Usernames of followers
followers = fromJSON("https://api.github.com/users/harryob2/followers")
followers$login

# Display the number of users I am following
myData$following

# Gives user names of all the users I am following
following = fromJSON("https://api.github.com/users/harryob2/following")
following$login

# Display the number of repositories I have
myData$public_repos

# Gives the name and creation date for my repositories
repositories = fromJSON("https://api.github.com/users/harryob2/repos")
repositories$name
repositories$created_at


#For this assignment I have used Fabien Potencier's Github Account - fabpot
#I used him because I looked up famous people on Github and his was the first to come up

#Prepare the data before running it:
allData = GET("https://api.github.com/users/fabpot", gtoken)
dataCont = content(allData)
followers = GET("https://api.github.com/users/fabpot/followers?per_page=100", gtoken)
followCont = content(followers)
repository = GET("https://api.github.com/users/fabpot/repos", gtoken)
repoCont = content(repository)

dataFrame = jsonlite::fromJSON(jsonlite::toJSON(dataCont))
followerFrame = jsonlite::fromJSON(jsonlite::toJSON(followCont))
repoFrame = jsonlite::fromJSON(jsonlite::toJSON(repoCont))

dataFrame$followers         #Num followers
dataFrame$public_repos      #Num public repos
dataFrame$login             #login name

length(followerFrame$login)
repoFrame$name              #Repo names
repoFrame$created_at        #Date repo created
# List of usernames
followerFrame$login        
user_ids = c(followerFrame$login)

# Create empty set
users = c()
usersDB = data.frame(username = integer(), following = integer(), followers = integer(), repos = integer(), dateCreated = integer())

#Add users to list
for(i in 1:length(user_ids))
{
  followURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followReq = GET(followURL, gtoken)
  followCont = content(followReq)
  
  if(length(followCont) == 0)
  {
    next
  }
  
  followDFrame = jsonlite::fromJSON(jsonlite::toJSON(followCont))
  followLog = followDFrame$login
  
  #Loop through users
  for (j in 1:length(followLog))
  {
    if (is.element(followLog[j], users) == FALSE)
    {
      users[length(users) + 1] = followLog[j] #Adds user to list
      
      followURL2 = paste("https://api.github.com/users/", followLog[j], sep = "")
      following2 = GET(followURL2, gtoken)
      followCont2 = content(following2)
      followDFrame2 = jsonlite::fromJSON(jsonlite::toJSON(followCont2))
      
      
      followingNumber = followDFrame2$following #following
      followersNumber = followDFrame2$followers #followers
      reposNumber = followDFrame2$public_repos  #Repo num
      yearCreated = substr(followDFrame2$created_at, start = 1, stop = 4) #year joined
      
      usersDB[nrow(usersDB) + 1, ] = c(followLog[j], followingNumber, followersNumber, reposNumber, yearCreated)
    }
    next
  }
  if(length(users) > 100) #stop after 100 entries
  {
    break
  }
  next
}



#Plotly is a seperate SaaS website for embedding cool charts in code. It's what the economist and ft use.
Sys.setenv("plotly_username"="harryob2")
Sys.setenv("plotly_api_key"="TJkeReYRhlpjG1mxPEta")

#Plot 1 - Comparing repositories to followers
plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers, text = ~paste("Followers: ", followers, "<br>Repositories: ", repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1

api_create(plot1, filename = "Repositories v Followers")

###Plot 2 - fabpot's followers' 10 most popular programming languages
languages = c()

for (i in 1:length(users))
{
  reposURL = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  repos = GET(reposURL, gtoken)
  reposContent = content(repos)
  reposDF = jsonlite::fromJSON(jsonlite::toJSON(reposContent))
  reposNames = reposDF$name
  
  #Loop through all the repositories of an individual user
  for (j in 1: length(reposNames))
  {
    
    reposURL2 = paste("https://api.github.com/repos/", users[i], "/", reposNames[j], sep = "")
    repos2 = GET(reposURL2, gtoken)
    reposContent2 = content(repos2)
    reposDF2 = jsonlite::fromJSON(jsonlite::toJSON(reposContent2))
    language = reposDF2$language
    
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}
#Puts 10 most popular languages in a table and barPlot
allLanguages = sort(table(languages), increasing=TRUE)
top10Languages = allLanguages[(length(allLanguages)-9):length(allLanguages)]
languageDF = as.data.frame(top10Languages)
barplotLanguages = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
barplotLanguages


###Plot3 graphs fabpot's following vs followers by year.
plot3 = plot_ly(data = usersDB, x = usersDB$following, y = usersDB$followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated)
plot3
api_create(plot3, filename = "Following vs Followers")
