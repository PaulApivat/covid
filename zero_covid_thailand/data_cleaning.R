# library 
library(tidyverse)

# load data
df <- read_csv("./raw/aranet_campaign.csv")


# data cleaning

# changing column names
df1 <- df %>%
    rename(
        timestamp = `Timestamp`,
        email = `อีเมล`,
        name = `ชื่อ-นามสกุล`,
        address = `ที่อยู่`,
        province = `จังหวัด`,
        postal_code = `รหัสไปรษณีย์`,
        phone = `เบอร์โทรศัพท์`,
        occupation = `อาชีพ`,
        organization = `ชื่อองค์กร/ธุรกิจของคุณ (ถ้ามี)`,
        motivation = `ทำไมคุณถึงอยากได้เครื่องวัดระดับ CO2? ช่วยอธิบายเพิ่มเติมเกี่ยวกับตัวคุณให้ทีมเราได้รู้จักหน่อยค่ะ และมองว่าจะนำเครื่องวัดไปทำประโยชน์อะไรบ้าง และใครจะได้รับผลประโยชน์ของการใช้เครื่องวัดตัวนี้?`,
        measure_location = `คุณมองว่าจะเอาเครื่องวัด CO2 ไปวัดระดับคาร์บอนไดออกไซด์ที่ไหนบ้าง?`,
        sharing_date = `คุณสามารถช่วยถ่ายรูปและแชร์ข้อมูลการวัดผล CO2 ได้เร็วที่สุดเมื่อไหร่?`,
        facebook = `Facebook ID`,
        twitter = `Twitter ID`,
        instagram = `Instagram ID`,
        blockdit = `Blockdit ID`,
        identity_consent = `หากคุณเป็นผู้ที่ถูกรับเลือก ทางทีมเราจะประกาศรายชื่อผู้รับเครื่องวัดระดับ CO2 เป็นเมือง ประเทศ และ ชื่อ Twitter/Facebook/Instagram ID ของคุณ ...นอกจากนี้ คุณยินยอมที่จะให้เราแชร์รายละเอียดต่อไปนี้หรือไม่ (โปรดเลือกเพื่อยินยอม)`
    )


# Exploratory & further cleaning

# group by province
df1 %>%
    group_by(province) %>%
    tally(sort = TRUE) %>%
    view()

# group by occupation
df1 %>%
    group_by(occupation) %>%
    tally(sort = TRUE) %>%
    view()

#####-------Cleaning Data: Province ---------#####

# clean all spelling variations of กรุงเทพ          

df1$province <- if_else(df1$province=="กรุงเทพมหานคร", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กทม", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุงเทพฯ", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กทม.", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="Bangkok", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="bangkok", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุงเทพๆ", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุงเทพทหานคร", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุงเทพมหานครฯ", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุงเทพฯ.", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุุงเทพ", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุงเทพ​มหานคร​", "กรุงเทพ", df1$province)
df1$province <- if_else(df1$province=="กรุงเทพฯ​", "กรุงเทพ", df1$province)

# clean all spelling variations of Chiangmai

df1$province <- if_else(df1$province=="Chiang Mai", "เชียงใหม่", df1$province)
df1$province <- if_else(df1$province=="เชียงใหมี", "เชียงใหม่", df1$province)


# clean all spelling variations of Nakorn Pathom

df1$province <- if_else(df1$province=="nakhon pathom", "นครปฐม", df1$province)

# clean all spelling variations of Suphanburi

df1$province <- if_else(df1$province=="Suphanburi", "สุพรรณบุรี", df1$province)

# clean all spelling variations of Nakorn Ratchasima
# NOTE: seemingly identical spelling but not grouped together

df1$province <- if_else(df1$province=="นครราชสีมา", "นครราชสีมา", df1$province)

# clean all spelling variations of Suphanburi

df1$province <- if_else(df1$province=="จ.นครศรีธรรมราช", "นครศรีธรรมราช", df1$province)

# clean all spelling variations of Samut Prakarn

df1$province <- if_else(df1$province=="จ.สมุทรปราการ", "สมุทรปราการ", df1$province)

# clean all spelling variations of Surat Thani

df1$province <- if_else(df1$province=="สุราษฏร์ธานี", "สุราษฎร์ธานี", df1$province)


#####-------Cleaning Data: Occupation ---------#####

# NOTE: Will use Top 31 Occupations as 'model'
# Rationale: Top 31 original occupations have n > 1

# e.g., บล็อกเกอร์ อินฟลู​เอนเซอร์ will absorb Content Creator     

df1 %>%
    group_by(occupation) %>%
    tally(sort = TRUE) %>%
    slice(1:31) %>%
    view()

df1$occupation <- if_else(df1$occupation=="ผู้แทนขายเครื่องทางการแพทย์ โรงพยาบาล", "บุคลากรทางการแพทย์ (แพทย์ พยาบาล อื่นๆ)", df1$occupation)

# Student
df1$occupation <- if_else(df1$occupation=="นักศึกษา", "นักเรียน นักศึกษา", df1$occupation)
df1$occupation <- if_else(df1$occupation=="นักศึกษาฟิสิกส์ ป.โท", "นักเรียน นักศึกษา", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พึ่งจบการศึกษา", "นักเรียน นักศึกษา", df1$occupation)

# Teacher 
df1$occupation <- if_else(df1$occupation=="นักวิชาการศึกษา", "ครู​ นักวิชาการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="บุคลากรทางการศึกษา", "ครู​ นักวิชาการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="อาจารย์", "ครู​ นักวิชาการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="อาจารย์มหาวิทยาลัยราชภัฏวไลยอลงกรณ์", "ครู​ นักวิชาการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="อาจารย์มหาวิทยาลัยและเจ้าของธุรกิจ", "ครู​ นักวิชาการ", df1$occupation)



# Generic Employee / Salaryman
df1$occupation <- if_else(df1$occupation=="พนักงานบริษัท", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงาน", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานเอกชน", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="นักศึกษา ป.โท (นิเทศ) และ พนักงานเอกชน", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงาน บ.", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงาน ประจำ", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานบ.เอกชน", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานโรงงานเอกชน", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="รับจ้างพนักงานเอกชน", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ส่งพัสดุเอกชน", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานบ.", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานบริษัท, ช่างภาพฟรีแลนด์", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานบริษัททรู", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานประจำ", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานรับจ้างทั่วไป", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานออฟฟิส", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานทั่วไป", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="รับจ้าง", "พนักงานบริษัทเอกชน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนง.บ.", "พนักงานบริษัทเอกชน", df1$occupation)






# Factory Worker
df1$occupation <- if_else(df1$occupation=="พนักงานบริษัท(โรงงานเย็บผ้า)", "พนักงานโรงงาน", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานบริษัทดูแลเรื่องอาชีวอนามัยภายในโรงงาน", "พนักงานโรงงาน", df1$occupation)

# State Enterprise
df1$occupation <- if_else(df1$occupation=="พนง รัฐวิสาหกิจ", "พนักงานรัฐวิสาหกิจ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนง รัฐวิสาหกิจ", "พนักงานรัฐวิสาหกิจ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนง.รัฐวิสาหกิจ", "พนักงานรัฐวิสาหกิจ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงาน​รัฐวิสาหกิจ​", "พนักงานรัฐวิสาหกิจ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ลูกจ้างรัฐวิสาหกิจ", "พนักงานรัฐวิสาหกิจ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานรัฐวิสากิจ", "พนักงานรัฐวิสาหกิจ", df1$occupation)

# Government / Government Ministries
df1$occupation <- if_else(df1$occupation=="ข้าราชการ กรมพินิจฯ", "ข้าราชการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ข้าราชการทหาร", "ข้าราชการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="เจ้าหน้าที่หน่วยงานราชการ", "ข้าราชการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานราชการ", "ข้าราชการ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="รับราชการ", "ข้าราชการ", df1$occupation)


df1$occupation <- if_else(df1$occupation=="พนักงานมหาวิทยาลัยของรัฐ", "เจ้าหน่วยงานอื่นของรัฐ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ลูกจ้างหน่วยงานรัฐ​", "เจ้าหน่วยงานอื่นของรัฐ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="สถานทูตสหรัฐอเมริกา", "เจ้าหน่วยงานอื่นของรัฐ", df1$occupation)

# Driver / Transport / Deliveryman

df1$occupation <- if_else(df1$occupation=="ขับรถโดยสารประจำทาง", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ขับรถส่งเฟอร์นิเจอร์", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="บริการรับจ้างรถสาธารณะ", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ผู้ช่วยพนักงานขับรถ", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ให้บริการรถโดยสารสาธารณะ", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="เดลิเวอรี่", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="Rider lineman", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ไรเดอรส่งอาหาร", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ไลน์แมน", "พนักงานขับรถ", df1$occupation)
df1$occupation <- if_else(df1$occupation=="แกร๊ปไรเดอร์ (ฟู๊ด)", "พนักงานขับรถ", df1$occupation)



# Business Owner / Small Medium Enterprises (SME)

df1$occupation <- if_else(df1$occupation=="เจ้าของธุรกิจ", "เจ้าของธุรกิจ SME", df1$occupation)
df1$occupation <- if_else(df1$occupation=="เจ้าของธุระกิจ และ อาสากู้ภัย", "เจ้าของธุรกิจ SME", df1$occupation)

# Engineers & Technicians / Electricians / 

df1$occupation <- if_else(df1$occupation=="วิศวกร​", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="วิศวกร, ที่ปรึกษา, ผู้บริหาร", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="วิศวกรโยธา​", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="Engineer", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="engineer Sale ติดตั้ง", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="system engineer", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ช่าง", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ช่างเทคนิค", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ช่างไฟ", "วิศวกร", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ช่างระบบ ในสนามบินสุวรรณภูมิ", "วิศวกร", df1$occupation)


# Sales, Retail, Marketing

df1$occupation <- if_else(df1$occupation=="แม่ค้าขายของในตลาดนัด", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ตัวแทนขายประกัน", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ที่ปรึกษาการขายรถยนต์", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="ธุรกิจส่วนตัวขายส่งเสื้อผ้า ขับไรเดอร์ส่งอาหาร", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานขาย", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="พนักงานขาย หาลูกค้า", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="อาชีพอิสระค้าขายในตลาด", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="Digital Marketing + Sales", "ค้าขาย", df1$occupation)
df1$occupation <- if_else(df1$occupation=="Marketing affiliate", "ค้าขาย", df1$occupation)


####### NOTE: Data Cleaning could continue here, but I'll move on in the interest of time #########
####### Occupations have been grouped in some broad categories, but idiosyncratic occupations are still prevalent ######

## FACEBOOK ID ##
# How many provided Facebook ID?        490 Responses,    135 NA or unusable responses

df1 %>%
    group_by(facebook) %>%
    tally(sort = TRUE) %>%
    view()


## Instagram ID ##
# How many provided Instagram ID?         178 Responses,    448 NA or unusable responses


df1 %>%
    group_by(instagram) %>%
    tally(sort = TRUE) %>%
    view()


## TWITTER ID ##
# How many provided Twitter ID?         105 Responses,    523 NA or unusable responses


df1 %>%
    group_by(twitter) %>%
    tally(sort = TRUE) %>%
    view()


## Blockdit ID ##
# How many provided Blockdit ID?         29 Responses,    599 NA or unusable responses


df1 %>%
    group_by(blockdit) %>%
    tally(sort = TRUE) %>%
    view()


### Basic Data Visualizations ###
# To get a feel for the data and allow ZCT to make decisions on how to distribute the Aranet machines

# First write csv
write_csv(df1, "cleaned_aranet_campaign.csv", append = FALSE, col_names = TRUE)


#### ------ BASIC VISUALIZATIONS -------- #####


# Thai font

# province

df1 %>%
    select(province) %>%
    group_by(province) %>%
    tally(sort = TRUE) %>%
    ggplot(aes(x = reorder(province, n), y = n)) +
    geom_col(aes(fill = province)) +
    theme(
        axis.text.y = element_text(family = "Krub", hjust = 1),
        legend.position = "none"
    ) +
    coord_flip()



# bangkok - postal_code

occupation

social media metrics















