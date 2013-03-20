#!/usr/bin/env python
'''
Created on Apr 17, 2012
@author: Vlad Gorloff

Modified by Dilawar for personal use.
'''
import argparse
import os
import sys
import getopt
import re
import blogger.BloggerUpdater as BloggerUpdater
      
def main(args):
  argv = sys.argv

  # Getting command line arguments   
  if not os.path.exists('/home/dilawar/.bloggerrc') :
    print("""Create a ~/.bloggerrc file with following lines. "
      blog=Dilawar's Blog
      user=username
      password=password
      """)
    sys.exit()

  file = open('/home/dilawar/.bloggerrc', 'r');
  for line in file :
    line = line.split('=')
    key = line[0]
    value = line[1]
    if key.strip() == "blog" :
      blog = value.strip()
    elif key.strip() == "user" :
      user = value.strip()
    elif key.strip() == "password" :
      password = value.strip()
    else :
      print key, "Unknown option"
      return 1

  # Creating instance of BloggerUpdater
  updater = BloggerUpdater.BloggerUpdater(user, password)
  # Getting blog entry
  blogEntry = updater.GetBlogByTitle(blog)
  if blogEntry is None:
     print "[E] Unable to find requested blog: \"" + blog + "\""
     return 1
  print "[I] Requested blog found: \"" + blogEntry.title.text + "\""

  if args.fetch :
    print("Fetching the post : {0}".format(args.fetch))
    post = updater.GetPostByTitle(args.fetch)
    if post :
      filename = (post.title.text).strip()
      filename = filename.replace(" ", "_")
      with open(filename+".html", 'w') as blog :
        blog.write("<title>\n")
        blog.write(post.title.text+"\n</title>")
        blog.write("\n\n")
        blog.write("<content>\n\n");
        blog.write(post.content.text)
        blog.write("\n\n</content>\n")
      return
    else :
      print "I can't find post named "+get
 
  elif args.send :  
    file = args.send
    if not os.path.exits(file) :
      print("File {0} does not exists".format(src))
      return
    txt = open(src, 'r').read()   # Opening source HTML for reading
    titleRegex = re.compile(r'\<title\>(?P<title>.+)\<\/title\>'
        , re.IGNORECASE | re.DOTALL)
    s = titleRegex.search(txt)
    if not s :
      print("I can not find the title. Existing ...")
      return 
      
    title = m.groupdict()['title']
    contentRegex = re.compile(r'\<content\>(?P<content>.+)\<\/content\>',
        re.IGNORECASE | re.DOTALL)
    s = contentRegex.search(txt)
    if not s :
      print("I can't find the content. Existing ...")
      return 
    content1 = s.groupdict()['content']
    # Removing whitespace between HTML tags    
    content1 = re.sub(r'>\s+<', "><", content1)    
    # Removing leading and trailing whitespace
    content = re.sub(r"[\n]+", "", content1)  
   
    # Getting post entry
    title = title.replace("\n","")
    postEntry = updater.GetPostByTitle(title)
    if postEntry is None:
        print "[E] Unable to find requested post: \"" + post + "\""
        return 1
    print("[I] Requested post found: {0}. Last update: {1}. Updating ..." \
        .format(postEntry.title.text, postEntry.updated.text))
    
    # Updating post with new content
    resultEntry = updater.UpdatePost(postEntry, content)
    print("[I] Successfully updated: {0}. Last update: {1}. Done!"\
        .format(resultEntry.title.text, resultEntry.updated.text)) 
    return 0
 
   
if __name__ == '__main__':
  parser = argparse.ArgumentParser(description="Blogger client")
  parser.add_argument('--fetch', metavar="f"
      , help="Fetch a post with similar looking name."
      )
  parser.add_argument('--send', metavar='s'
      , help="Update a post."
      )
  args = parser.parse_args()
  main(args)
