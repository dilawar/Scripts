#!/usr/bin/env python
'''
Created on Apr 17, 2012
Originall written by : Vlad Gorloff

Modified by Dilawar for personal use.
'''

import sys
if sys.version_info < (3, 0) :
  from ConfigParser import RawConfigParser
else :
  from configparser import RawConfigParser 

from wordpress_xmlrpc import Client, WordPressPost
from wordpress_xmlrpc.methods.posts import GetPosts, NewPost, EditPost
from wordpress_xmlrpc.methods.users import GetUserInfo
import argparse
import os
import re
import lxml.html as lh
import codecs
import errno
import difflib 
import os

# globals 
blogDir = "."


def formatWithNoChangeOnTag(txt, tag) :
  ''' Notice :
  +QUQ+ should be replace by \n after using this function last time.
  '''
  newText = ""
  bTag = False 
  eTag = True
  beginTag = re.compile("[\<\[]\s*"+tag+"\s*(\w+\s*=\s*[\"\w\']+\s*)?[\>\]]",
      re.IGNORECASE)
  endTag = re.compile("[\<\[]\s*\/"+tag+"\s*[\>\]]", re.IGNORECASE)
  for line in txt.split("\n") :
    if len(line.strip()) == 0 : continue 
    if beginTag.search(line) and endTag.search(line) : newText += line
    else : 
      if beginTag.search(line) :
        newText += "\n"
        bTag = True
        eTag = False
      if endTag.search(line) :
        eTag = True
        bTag = False
    #check 
    if bTag is True and eTag is False:
      newText += (line+"+QUQ+")
    else : # format it 
      newText += (line.strip()+"\n")
  return newText

def formatContent(content) :
  content = formatWithNoChangeOnTag(content, "sourcecode")
  content = formatWithNoChangeOnTag(content, "pre")
  content = re.sub("\n+", " ", content)
  content = content.replace("+QUQ+", "\n")
  return content

def getTitle(txt) :
  titleRegex = re.compile("\<TITLE\>\s*(?P<title>[\w\W\s]+)\s*\</TITLE\>",
        re.IGNORECASE | re.DOTALL)
  m = titleRegex.search(txt)
  if m :
    title = m.groupdict()['title']
  else :
    print("[W] Empty title!")
    title = ""
  return title

def titleToFileName(title) :
  global blogDir 
  fileName = title.replace(" ","_")+".blog"
  fileName = fileName.replace("/", "_")
  fileName = os.path.abspath(blogDir+"/"+fileName)
  return fileName


def sendPostToWordpress(post, wp, txt) :
  # Check if there is no id.
  idregex = re.compile('\<ID\>\s*(?P<id>\d+)\s*\</ID\>', re.IGNORECASE | re.DOTALL)
  m = idregex.search(txt) 
  if not m :
    print("This looks like a new post, use --post option")
    return 
  else :
    id = m.groupdict()['id']
    post.id = id
    title = getTitle(txt)
    post.title = title
    
    print("[I] Sending post : {0} : {1}.".format(id, title))

    # content 
    contentRegex = re.compile("\<CONTENT\>(?P<content>.+)\<\/CONTENT\>"
        , re.IGNORECASE | re.DOTALL)
    m = contentRegex.search(txt)
    if m :
      content = m.groupdict()['content']
      if len(content.strip()) == 0 :
        print("[E] : No content in file.")
        return 
      content = formatContent(content)
    else :
      print("[W] Post with empty content.")
      content = ""
    post.content = content
    
    # status 
    statusRegex = re.compile("\<STATUS\>\s*(?P<status>\w+)\s*\</STATUS\>"
        , re.IGNORECASE | re.DOTALL)
    m = statusRegex.search(txt)
    if m :
      status = m.groupdict()['status']
      post.post_status = status 
    else :
      print("[W] Post with uncertain status. Default to publish")
      pagepost.post_status = "publish"
    
    termsAndCats = dict()
    
    # tags 
    tagRegex = re.compile("\<POST_TAG\s+ID\=\"(?P<id>\d*)\"\>\s*"+\
        "(?P<tag>[\s\w]+)\s*\</POST_TAG\>", re.IGNORECASE | re.DOTALL)
    ms = tagRegex.findall(txt)
    tags = list()
    for m in ms :
      id, name = m
      tags.append(name)
    termsAndCats['post_tag'] = tags 

    # categories
    catRegex = re.compile("\<CATEGORY\s+ID\=\"(?P<id>\d*)\"\>\s*"+\
        "(?P<cat>[\s\w]+)\s*\</CATEGORY\>", re.IGNORECASE | re.DOTALL)
    mm = catRegex.findall(txt)
    cats = list()
    for m in mm :
      id, cat = m
      cats.append(cat)
    termsAndCats['category'] = cats

    post.terms_names = termsAndCats 
    wp.call(EditPost(post.id, post))
    return


def fetchPosts(posts, type) :
  """ Fetch all posts in list posts with type type
  """
  global blogDir
  for post in posts :
    title = post.title.encode('utf-8')
    terms = post.terms
    print("[I] : Downloading : {0}".format(title))
    content = post.content 
    content = content.replace("<br/>", "<br>")
    content = content.replace("<br />", "<br>")
    content = content.replace("<br>", "\n<br>\n")
    content = content.replace("<pre>", "\n\n<pre>") 
    content = content.replace("</pre>", "</pre>\n\n") 
    content = content.replace("<p>", "\n\n<p>")
    content = content.replace("[/sourcecode", "\n\n[/sourcecode")
    fileName = titleToFileName(title)
    f = codecs.open(fileName, "w", encoding="utf-8", errors="ignore")
    f.write("<TYPE>"+type+"</TYPE>\n")
    f.write("<STATUS>"+post.post_status+"</STATUS>\n")
    f.write("<ID>"+post.id+"</ID>\n")
    f.write("<TITLE>\n")
    f.write(title)
    f.write("\n</TITLE>\n\n")
    f.write("<CONTENT>\n")
    f.write(content)
    f.write("\n\n</CONTENT>\n")
    for t in terms :
      f.write("\n<"+t.taxonomy.upper()+" ID=\""+t.taxonomy_id+"\">"\
          +t.name+"</"+t.taxonomy.upper()+">\n")
    
    f.close()


def main(args):
  # Getting command line arguments   
  global blogDir
  configFilePath = os.getenv('HOME')+"/.wordpressrc"
  if not os.path.exists(configFilePath) :
    print("""
Create a ~/.wordpressrc file with following lines. "
[blog]
url=http://dilawarrajput.wordpress.com"
username=username
password=password
      """)
    sys.exit()

  cfg = RawConfigParser()
  with open(configFilePath, "r") as configFile :
    cfg.readfp(configFile)
  blogId = "blog"+str(args.blog)
  blog = cfg.get(blogId, 'url')
  blog = blog.replace("www.", "")
  blog = blog.replace("http://", "")
  blogDir = blog.replace(".", "DOT")
  blog = blog.replace("/xmlrpc.php", "")
  blog = "http://"+blog+"/xmlrpc.php"
  user = cfg.get(blogId,'username')
  password = cfg.get(blogId, 'password')

  try :
    os.makedirs(blogDir)
  except OSError as exception :
    if exception.errno != errno.EEXIST :
      raise
  
  ## Now cleate a client 
  wp = Client(blog, user, password, proxy=os.environ['http_proxy'])
  
  # Send a file to wordpress.
  if args.update :
    fileName = args.update
    if not os.path.exists(fileName) :
      print("File {0} doesn't exists.. Existing...".format(fileName))
      return 
    # Open the file.
    with open(fileName, "r") as f :
      txt = f.read()
    post = WordPressPost()
    sendPostToWordpress(post, wp, txt) 
  
  elif args.post :
    print("You are going to create a new post.")
    post = WordPressPost()
    post.id = wp.call(NewPost(post))
    # get the text of new post
    fileName = args.post 
    with open(fileName, "r") as f :
      txt = f.read()
    
    txt = "<ID>"+post.id+"</ID>\n" + txt
    title = getTitle(txt)
    savePath = titleToFileName(title)
    print("|- Adding id to new post and saving it to : \n {0}".format(savePath))
    with open(savePath, "w") as ff :
      ff.write(txt)
    sendPostToWordpress(post, wp, txt)
    print("== You should now delete : {0}.".format(args.post))
    return
    
  # Fetch blogs from wordpress.
  elif args.fetch :
  # Get all posts 
    posts = wp.call(GetPosts(
      {'number': 200, 'offset': 0}
      ))
    pages = wp.call(GetPosts({'post_type' : 'page'}))
    if  args.fetch == "all" :
      fetchPosts(posts, "post")
      fetchPosts(pages, "page")
    elif len(args.fetch) > 2 :
      # search for a post with similar titles.
      matchedPosts = list()
      for post in posts :
        title = post.title 
        match = difflib.SequenceMatcher(None, title, args.fetch).ratio()
        if match > 0.65 :
          matchedPosts.append(post)
      fetchPosts(matchedPosts, "post")

      # Why not pages.
      matchedPages = list()
      for page in pages :
        title = page.title 
        match = difflib.SequenceMatcher(None, title, args.fetch).ratio()
        if match > 0.65 :
          matchedPages.append(post)
      fetchPosts(matchedPages, "page")

    else : # get recent posts 
      posts = wp.call(GetPosts(
        {'post_status': 'publish'}
        ))
      fetchPosts(posts, "post")
  
 
if __name__ == '__main__':
  parser = argparse.ArgumentParser(description="Blogger client")
  parser.add_argument('--blog', metavar="blog index in config file eg. 0, 1"
      , default = "0"
      , help = "Index of blog. If not given 0 is assumed"
      )
  parser.add_argument('--fetch', metavar="[all|post_name]"
      , help="Fetch a post with similar looking name. If 'recent' is given, it  \
          fetch and save recent posts. If 'all' is given then it fetches all\
          posts "
      )
  parser.add_argument('--update', metavar='blog_file'
      , help="Update a post."
      )
  parser.add_argument('--post', metavar='blog_file'
      , help="New post or page"
      )
  args = parser.parse_args()
  main(args)
