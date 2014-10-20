BEGIN
 READ file
 DELETE lines with GREP "The Directory of Middle East & North African Libraries"
 DELETE all lines before "PAGE 4"
 DELETE lines with GREP "PAGE"
 
 DELETE lines with GREP "foreword"
 DELETE lines with "Foundation Profiles"
 FIND "CONTENT"
 CREATE vector countries
  FOR each line after "CONTENT" until "INDEX"
   GREP all before periods -> x
   APPEND to countries
 
 CREATE data.frame with columns("name","country","tel","email","web","head","books","journals","subjects")
 
 FIND "index"
  FOR i line after "index"
   GREP text before periods -> i
   CREATE i row in data.frame
   
    
END