#!/usr/bin/env python
'''
Created on Apr 17, 2012
@author: Vlad Gorloff

Modified by Dilawar for personal use.
'''

import os
import sys
import getopt
import re
import BloggerUpdater

def usage():
    print >>sys.stderr, "Usage:"
    print >>sys.stderr, "-h, --help - this help"
    print >>sys.stderr, "-b, --blog - name of existing blog"
    print >>sys.stderr, "-p, --post - name of existing post"
    print >>sys.stderr, "-s, --src  - path to HTML file with replacement content"
    print >>sys.stderr, "-g, --get  - name of blog to retrieve"
    print >>sys.stderr, "--user     - Blogger account name"
    print >>sys.stderr, "--pass     - Blogger account password"
    print >>sys.stderr, ""
      
def main(argv=None):
    if argv is None:
        argv = sys.argv
    
    # Getting command line arguments   
    try:
      opts, args = getopt.getopt(argv[1:], "p:s:g:h",
                                   ["post", "src", "get" "help"])
    except getopt.GetoptError, err:
        print str(err)
        usage()
        return 1

    if len(opts) <= 0: # Inconsistent number of command line arguments
        usage()
        return 1
    
    # Retrieving arguments
    get = None;
    src = None;
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            return 0
        elif o in ("-p", "--post"):
            post = a
        elif o in ("-s", "--src"):
            src = a        
        elif o in ("-g", "--get"):
            get = a
        else:
            print "unknown option: " + o
            usage()
            return 1
        
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

    if get :
      post = updater.GetPostByTitle(get)
      fileName = (post.title.text).strip()
      fileName = re.sub(r"\s+", "", fileName)
      f = open(fileName+".html", 'w')
      f.write(post.content.text)
      f.close()
      return
   
    elif src :  
      content1 = open(src, 'r').read()              # Opening source HTML for reading
      content1 = re.sub(r'>\s+<', "><", content1)    # Removing whitespace between HTML tags
      content = re.sub(r"[\n]+", "", content1)  # Removing leading and trailing whitespace
     
            # Getting post entry
      postEntry = updater.GetPostByTitle(post)
      if postEntry is None:
          print "[E] Unable to find requested post: \"" + post + "\""
          return 1
      print "[I] Requested post found: \"%s\". Last update: %s. Updating ..." \
          % (postEntry.title.text, postEntry.updated.text)
      
      # Updating post with new content
      resultEntry = updater.UpdatePost(postEntry, content)
      print "[I] Successfully updated: \"%s\". Last update: %s. Done!" \
          % (resultEntry.title.text, resultEntry.updated.text) 
      
      return 0
   
   
if __name__ == '__main__':
    sys.exit(main())
