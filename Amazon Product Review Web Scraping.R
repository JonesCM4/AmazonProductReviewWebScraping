library(tidyverse)
library(rvest)

#max number of pages is 10 (100 reviews per size and product color)
page <- 1:10

#product IDs as a list, example: B0C7C171Y5
#product IDs are unique by product color
code <- list()

#initialize df
final_df <- data.frame(title = character(), text = character(), star = numeric(), stringsAsFactors = FALSE)

#retrieves amazon product review title, text, and stars 
product_reviews = function(code, page) {
  for (c in code) {
    for (p in page) {
    #sort by recent reviews, change url
    url <- paste0("https://www.amazon.com/.../product-reviews/", c,
                  "/...&reviewerType=all_reviews&formatType=current_format&pageNumber=", p,
                  "&reviewerType=all_reviews&sortBy=recent")
    
    #read webpage
    html = read_html(url)
      
    #review title, change element
    title = html %>%
      html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
      html_text2()
      
    #review text, change element
    text = html %>%
      html_elements("[class='a-size-base review-text review-text-content']") %>%
      html_text2()
      
    #review stars, change element
    star = html %>%
      html_elements("[data-hook='review-star-rating']") %>%
      html_text2()
    
    #initialize df
    df <- data.frame(title = title, text = text, star = star, stringsAsFactors = FALSE)
    
    #append to final_df
    final_df <- rbind(final_df, df)
      
    }
  }
  return(final_df)
}

#call function
df <- product_reviews(code, page)

#remove duplicates
df <- df[!(duplicated(df)),]
