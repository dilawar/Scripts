#!/usr/bin/env python

import argparse
import os 

import sys
if sys.version_info < (3, 0) :
  from ConfigParser import RawConfigParser
else :
  from configparser import RawConfigParser 

from wordpress_xmlrpc import Client, WordPressPost
from wordpress_xmlrpc.methods.posts import GetPosts, NewPost, EditPost
from wordpress_xmlrpc.methods.users import GetUserInfo
from wordpress_xmlrpc.methods import media, posts 

import argparse
import os
import re
import codecs
import errno
import difflib 
import subprocess

blogDir = "./blogs"

def newPostToWordpress(wp, postName):
    print("[INFO] You are going to create a new post ...")
    post = WordPressPost()
    post.id = wp.call(NewPost(post))
    # get the text of new post
    fileName = postName
    with open(fileName, "r") as f :
        txt = f.read()
    
    txt = "<id>"+post.id+"</id>\n" + txt
    title = getTitle(txt)
    savePath = titleToBlogDir(title)+'/content.md'
    with open(savePath, "w") as ff:
        ff.write(txt)
    updatePost(post, wp, txt)
    print(("== You should now delete : {0}.".format(postName)))
    return 0

def fetchWpPosts(wp, postsToFetch):
    """
    Fetch given posts from wordpress.
    """
     
    # Create blog directory if not exists.
    try :
        os.makedirs(blogDir)
    except OSError as exception :
        if exception.errno != errno.EEXIST :
            raise 
    
    posts = wp.call(GetPosts( {'number': 200, 'offset': 0}))
    pages = wp.call(GetPosts({'post_type' : 'page'}))
    if  postsToFetch == "all" :
        fetchPosts(posts, "post", wp)
        fetchPosts(pages, "page", wp)
    elif len(postsToFetch) > 2 :
        # search for a post with similar titles.
        matchedPosts = list()
        for post in posts :
            title = post.title 
            match = difflib.SequenceMatcher(None, title, postsToFetch).ratio()
            if match > 0.65 :
                matchedPosts.append(post)
        fetchPosts(matchedPosts, "post", wp)
        # Why not pages.
        matchedPages = list()
        for page in pages :
            title = page.title 
            match = difflib.SequenceMatcher(None, title, postsToFetch).ratio()
            if match > 0.65 :
                matchedPages.append(post)
        fetchPosts(matchedPages, "page", wp)
  

def getTitle(txt):
    titleRegex = re.compile("title:(?P<title>.+)", re.IGNORECASE)
    m = titleRegex.search(txt)
    if m :
        title = m.groupdict()['title']
    else :
        print("[W] Empty title!")
        title = ""
        
    return title.strip()
  
def titleToBlogDir(title):
    global blogDir
    fileName = title.replace(" ","_").replace(':', '-').replace('(', '')
    fileName = fileName.replace("/", "_").replace(')', '')
    fileName = os.path.join(blogDir, fileName)
    return fileName
  
def appendMetadataToPost(metadata, post):
    """
    Append metadata to post.
    """
    idregex = re.compile(r'id:(?P<id>.+)', re.IGNORECASE)
    m = idregex.search(metadata) 
    if not m :
        print("[Warning] This looks like a new post, use --post option")
        sys.exit()

    id = m.group('id').strip()
    post.id = id
    title = getTitle(metadata)
    post.title = title

    # status 
    statusRegex = re.compile("status:(?P<status>.+)", re.IGNORECASE) 
    m = statusRegex.search(metadata)
    if m :
        status = m.groupdict()['status']
        post.post_status = status 
    else :
        print("[W] Post with uncertain status. Default to publish")
        post.post_status = "publish"
    
    termsAndCats = dict()

    # tags 
    tagRegex = re.compile("tag:(?P<name>.+)", re.IGNORECASE)
    ms = tagRegex.findall(metadata)
    tags = list()
    for m in ms :
        name = m
        tags.append(name)
    termsAndCats['post_tag'] = tags 
  
    # categories
    catRegex = re.compile("category:(?P<cat>.+)", re.DOTALL)
    mm = catRegex.findall(metadata)
    cats = list()
    for m in mm :
        cat = m
        cats.append(cat)
    termsAndCats['category'] = cats
    post.terms_names = termsAndCats 
    return post

def updatePost(post, wp, txt, format="markdown") :
    # Check if there is no id.
    pat = re.compile(r'~~~+(?P<metadata>.+?)~~~+', re.DOTALL)
    metadata = pat.search(txt).group('metadata')
    content = re.sub(pat, "", txt)
    assert len(metadata) > 0
    
    post = appendMetadataToPost(metadata, post)
  
    # content 
    if content :
        if len(content.strip()) == 0 :
            print("[E] : No content in file.")
            return 
    else :
        print("[W] : Post with empty content.")
        content = ""

    if format == "html":
        pass
    elif format in ["markdown", "md"]:
        cmd = ["pandoc", "-f", "markdown", "-o", "html"]
        p = subprocess.Popen(cmd
                , stdin = subprocess.PIPE
                , stdout = subprocess.PIPE
                )
        p.stdin.write(content)
        post.content = p.communicate()[0]
    else:
        post.content = content

    print(("[I] Sending post : {0} : {1}.".format(post.id, post.title)))
    wp.call(EditPost(post.id, post))
    return

def writeContent(fH, content, format):
    if format == "html":
        fH.write(content)
    elif format in ["markdown", "md"]:
        p = subprocess.Popen(["pandoc", "-f", "html", "-t", "markdown"]
            , stdin=subprocess.PIPE
            , stdout=fH
            )
        p.communicate(content)


def fetchPosts(posts, postType, wp, format="markdown"):
    """ Fetch all posts in list posts with postType
    """
    global blogDir
    for post in posts :
        title = post.title.encode('utf-8')
        terms = post.terms
        print(("[I] : Downloading : {0}".format(title)))
        content = post.content.encode('utf-8') 
        postDir = titleToBlogDir(title)
        # Create directory for this filename in blogDir.
        if not os.path.isdir(postDir):
            os.makedirs(postDir)

        # Good now for this post, we have directory. Download its content in
        # content.md file.
        fileName = os.path.join(postDir, 'content.md')
        f = codecs.open(fileName, "w", encoding="utf-8", errors="ignore")
        f.write("~~~~ \n")
        f.write("title: ")
        f.write(title)
        f.write("\ntype: "+postType)
        f.write("\nstatus: "+post.post_status)
        f.write("\nid: "+post.id)
        cats = []
        tags = []
        for t in terms :
            if t.taxonomy == 'post_tag':
                tags.append(t.name)
            elif t.taxonomy == 'category':
                cats.append(t.name)
            else:
                cats.append(t.name)
        if tags:
            for t in tags:
                f.write('\ntag: {0}'.format(t)) 
        if cats:
            for c in cats:
                f.write('\ncategory: {0}'.format(c))
        f.write('\n')
        f.write("~~~~\n\n")
        # TODO: Get links from the post
        # Write content to file.
        writeContent(f, content, format)
        f.close()

def run(args):
    # Getting command line arguments   
    global blogDir 
    configFilePath = args.config
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
     ## Now cleate a client 
    p = os.environ.get('http_proxy')
    if p and 'http://' in p :
        p = p.replace('http://', '')
    else:
       p = ''

    wp = Client(blog, user, password, proxy=p)
    
    # Send a file to wordpress.
    if args.update :
        fileName = args.update
        if not os.path.exists(fileName):
            print(("File {0} doesn't exists.. Existing...".format(fileName)))
            return 
        # Open the file.
        with open(fileName, "r") as f:
            txt = f.read()
        post = WordPressPost()
        assert post is not None
        updatePost(post, wp, txt, format="markdown") 
    elif args.post :
        newPostToWordpress(wp, args.post)
    # Fetch blogs from wordpress.
    elif args.fetch :
        # Get all posts 
        fetchWpPosts(wp, args.fetch)
    else : # get recent posts 
        posts = wp.call(GetPosts( {'post_status': 'publish'}))
        fetchPosts(posts, "post", wp)
        


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Blogger client")
    parser.add_argument('--config', metavar="config"
        , default = os.environ['HOME'] + "/.wordpressrc"
        , help = "Config file containing setting. Default ~/.wordpressrc"
        )
    parser.add_argument('--blog', metavar="blog index in config file eg. 0, 1"
        , default = "0"
        , help = "Index of blog. If not given 0 is assumed"
        )
    parser.add_argument('--proxy', metavar="proxy"
        , help = "Setup proxy information.")
  
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
    run(args) 
