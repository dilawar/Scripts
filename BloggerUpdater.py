'''
Created on Apr 17, 2012
@author: Vlad Gorloff
'''

import difflib
from gdata import service
import atom
import sys

"""Simple class for updating posts on www.blogger.com service"""
class BloggerUpdater:
    
    """Initializing instance and login into www.blogger.com"""
    def __init__(self, user, password):
        self.blogger_service = service.GDataService(user, password)
        self.blogger_service.source = 'gv-cl-blogger-updater-1.0'   # Client software name
        self.blogger_service.service = 'blogger'
        self.blogger_service.account_type = 'GOOGLE'
        self.blogger_service.server = 'www.blogger.com'
        self.blogger_service.ssl = True                             # Let's protect connection
        print "[I] Logging into " + self.blogger_service.server + " ..."
        self.blogger_service.ProgrammaticLogin()
        if self.blogger_service.current_token is None:
            print "[E] Unable to login into \"" + self.blogger_service.server + "\""
    
    """This will get blog entry by it's title (name)"""  
    def GetBlogByTitle(self, title):
        query = service.Query()
        query.feed = '/feeds/default/blogs'
        feed = self.blogger_service.Get(query.ToUri())
        for entry in feed.entry:
            if entry.title.text == title:
                self.blog_id = entry.GetSelfLink().href.split("/")[-1]
                return entry
        print("Can't find blog with title : {0}".format(title))
        sys.exit(0)
    
    """This will get post entry by it's title (name)"""
    def GetPostByTitle(self, title):
        feed = self.blogger_service.GetFeed('/feeds/' + self.blog_id + '/posts/default')
        for entry in feed.entry:
          if entry.title.text :
            match = difflib.SequenceMatcher(None, entry.title.text 
                ,title).ratio()
            if match > 0.6 :
              print(" |- Found with title : {0} ".format(entry.title.text))
              return entry
            else : pass
          else : pass
        print("|- Can't find post with this title.")
        return None
    
    """This will update supplied post entry with new content
    Updated post entry returned as result"""
    def UpdatePost(self, postEntry, newContent):
        contentType = postEntry.content.type
        postEntry.content = atom.Content(contentType, None, newContent)
        return self.blogger_service.Put(postEntry, postEntry.GetEditLink().href)

