refine <- read.csv("C:/Users/USTOJOS/refine_original.csv")

View(refine)

refined <- 
  refine %>%
    #Clean up brand names
      mutate(company=tolower(company))%>%
      mutate(company=ifelse(grepl("^[Aa][Kk].*",refine$company),"akzo",
                        ifelse(grepl("^([Ff]|[Pp][Hh].*)",refine$company),"philips",
                               ifelse(grepl("^[Vv][Aa].*",refine$company),"van houten",
                                                         ifelse(grepl("^[Uu][Nn].*",refine$company),"unilever","NA"))))) %>%
  #Separate product code and number
    rename(ProductCode=Product.code...number) %>%
    separate(ProductCode, c("product_code","product_number")) %>%
  #Add product categories
    mutate(product_category=ifelse(product_code=="p","Smartphone",
                                 ifelse(product_code=="v","TV",
                                        ifelse(product_code=="x","Laptop",
                                               ifelse(product_code=="q","Tablet","NA"))))) %>%
  #Add full address for geocoding
    unite("Address",address,city,country, sep=",",remove=FALSE) %>%
    arrange(company,product_code,name) %>%
  #Create dummy variables for company
    mutate(company_philips=ifelse(company=="philips",1,0)) %>%
    mutate(company_akzo=ifelse(company=="akzo",1,0)) %>%
    mutate(company_van_houten=ifelse(company=="van houten",1,0)) %>%
    mutate(company_unilever=ifelse(company=="unilever",1,0)) %>%
  #Create dummy variables for product category
    mutate(product_smartphone=ifelse(product_category=="Smartphone",1,0)) %>%
    mutate(product_tv=ifelse(product_category=="TV",1,0)) %>%
    mutate(product_laptop=ifelse(product_category=="Laptop",1,0)) %>%
    mutate(product_tablet=ifelse(product_category=="Tablet",1,0))
  
View(refined)

write.table(refined, file="C:/Users/USTOJOS/refine_clean.csv", sep=",",row.name=FALSE)
