#!/usr/bin/python3

# The client_id and client_secret are copied from the API Access tab on
# the Google APIs Console
CLIENT_ID = "852604153947-1fsthdfrrfrgoe3keuei79sk0toal60n.apps.googleusercontent.com"
CLIENT_SECRET = "Md2N_4CYCZtH8eYCfB8oA76u"

""" TODO.TXT Google Task Sync
USAGE:
    google [pull|push|push all]

SETUP AND CONFIGURATION:
  Enable Google API Access
    - Create a project in the Google APIs Console
    (http://code.google.com/apis/console).
    - Under "APIs" enable the Tasks API
    - Under "Credentials" create a new Client Id for an installed application
    - Enter the Google-provided Client ID and Client Secret in the variables
    at the start of the addon script.

  Install Dependencies

    pip install google-api-python-client

USAGE NOTES:
    Note that for simplicity, the 'archive' command is executed prior to the
    sync.

    The 'pull' command will fetch google tasks and add them to your todo and
    one lists. Tasks will be pulled from all Google Task lists

    The 'push' command will push your todo list from todo.txt to Google Tasks
    The 'push all' command will push items from both todo.txt and done.txt
    Currently all new tasks will be added to the default Google Task list.
    (This may change in the future)

    As this is a pull/push, there are obviously limitations to the syncing.
    For instance an already pushed  todo that is editted considerably will get
    pushed as a separate task as there is no way to match old and new. I've
    tried to allow for some of the more common and minor changes, such as
    marking a todo with a priority. Also, completing a todo or task can be
    synced up or down accordingly.
"""


import os
import sys
import httplib2
import re
import codecs
from datetime import datetime

from apiclient.discovery import build
from oauth2client.file import Storage
from oauth2client.client import OAuth2WebServerFlow
from oauth2client.tools import run
from subprocess import call

__version__ = "0.1"
__date__ = "2011/06/06"
__updated__ = "2012/01/18"
__author__ = "Andrew McIntosh (github.com/amcintosh)"
__copyright__ = "Copyright 2012, Andrew McIntosh"
__license__ = "GPL"

# Set up a Flow object for authentication. Uses OAuth 2.0
FLOW = OAuth2WebServerFlow(
    client_id=os.environ['GOOGLE_API_CLIENT_ID'],
    client_secret=os.environ['GOOGLE_CLIENT_SECRET'],
    scope='https://www.googleapis.com/auth/tasks',
    user_agent='todo.txt/1.0')

def usage():
    """Prints progam usage"""
    print("USAGE: google [pull|push|push all]")


def getAPIService():
    """Authorize and return a session for the Tasks API"""
    # Check is credentials are stored and valid. Otherwise get credentials
    storage = Storage(".tasks.dat")
    credentials = storage.get()
    if credentials is None or credentials.invalid:
        credentials = run(FLOW, storage)

    http = httplib2.Http()
    http = credentials.authorize(http)

    # Build a service object for interacting with the API.
    service = build(serviceName="tasks", version="v1", http=http)
    return service


def addToTodo(task):
    """Format the google task and add it to todo file"""
    todo = task["title"]
    call([os.environ.get("TODO_FULL_SH"), "add", todo])


def addToDone(task):
    """Format the google task and add it to todo done file"""
    date = datetime.strptime(task["completed"], "%Y-%m-%dT%H:%M:%S.%fZ")
    todo = "x " + date.strftime("%Y-%m-%d") + " " \
        + re.sub("\([A-Z]\) ", "", task["title"], 1)
    call([os.environ.get("TODO_FULL_SH"), "addto", "done.txt", todo])


def addToTasks(service, todo):
    """Adds the todo data as a Google Task"""
    if todo[:1].lower() == "x":
        task = {
            "title": todo[13:],
            "status": "completed",
            "completed": todo[2:12]+"T12:00:00.000Z"
        }
    else:
        task = {
            "title": todo,
            "status": "needsAction"
        }
    service.tasks().insert(tasklist='@default', body=task).execute()


def updateTask(service, tasklistId, task, todo):
    """Updates a task with the current todo title"""
    if todo[:1].lower() == "x":
        task["title"] = todo[13:]
    else:
        task["title"] = todo
    service.tasks().update(tasklist=tasklistId,
                           task=task['id'],
                           body=task).execute()


def completeTask(service, tasklistId, task, todo):
    """Marks and existing task as completed"""
    task["status"] = "completed"
    task["completed"] = todo[2:12] + "T12:00:00.000Z"
    service.tasks().update(tasklist=tasklistId,
                           task=task['id'],
                           body=task).execute()


def pull():
    """Get Tasks from google task API and add those that don't
    exist to todo.txt
    """
    service = getAPIService()
    tasklists = service.tasklists().list().execute()
    call([os.environ.get("TODO_FULL_SH"), "archive"])
    try:
        todoFile = codecs.open(os.environ.get("TODO_DIR")+"/todo.txt", "r",
                               encoding='utf-8')
        doneFile = codecs.open(os.environ.get("TODO_DIR")+"/done.txt", "r",
                               encoding='utf-8')

        todos = []
        dones = []
        for line in todoFile:
            todos.append(line)
        for line in doneFile:
            dones.append(line)
        todoFile.close()
        doneFile.close()

    except IOError:
        print( "ERROR: The todo.txt files could not be read." )
        sys.exit(2)

    for tasklist in tasklists["items"]:
        # tasklistName = tasklist["title"]
        tasks = service.tasks().list(tasklist=tasklist["id"]).execute()

        for task in tasks["items"]:

            if task["title"].strip():
                # Check for the task in the todo files
                foundInTodo = -1
                foundInDone = -1
                count = 1
                for item in todos:
                    if task["title"].strip() in item:
                        foundInTodo = count
                        break
                    count += 1
                count = 1
                for item in dones:
                    if re.sub("\([A-Z]\) ", "", task["title"].strip(), 1) \
                            in item:
                        foundInDone = count
                        break
                    count += 1

                # Decide what to do with task
                if task["status"] == "needsAction" and foundInTodo > -1:
                    # Do nothing
                    pass
                elif task["status"] == "completed" and foundInTodo > -1:
                    # Remove from todo.txt and add to done.txt
                    call([os.environ.get("TODO_FULL_SH"),
                          "do",
                          str(foundInTodo)])
                elif task["status"] == "needsAction" and foundInDone > -1:
                    # Do nothing (changes need synced up, but not my problem)
                    pass
                elif task["status"] == "completed" and foundInDone > -1:
                    # Do nothing
                    pass
                elif task["status"] == "needsAction":
                    # Add to todo.txt
                    addToTodo(task)
                elif task["status"] == "completed":
                    # Add to done.txt
                    addToDone(task)


def push(filename):
    """Get items from todo.txt and adds them to google tasks via API"""
    service = getAPIService()
    tasklists = service.tasklists().list().execute()
    call([os.environ.get("TODO_FULL_SH"), "archive"])

    try:
        todoFile = codecs.open(
                    os.environ.get("TODO_DIR") + "/" + filename + ".txt", "r",
                    encoding='utf-8')

        todos = []
        for line in todoFile:
            todos.append(line)
        todoFile.close()

    except IOError:
        print( "ERROR: The " + filename + ".txt files could not be read." )
        sys.exit(2)

    for todo in todos:
        found = False
        for tasklist in tasklists["items"]:
            # tasklistName = tasklist["title"]
            tasks = service.tasks().list(tasklist=tasklist["id"]).execute()
            for task in tasks["items"]:
                if task["title"].strip() and \
                    (task["title"].strip() in todo
                        or (todo[:1].lower() == "x"
                            and todo[13:].strip() in task["title"].strip())):
                    found = True
                    if todo[:1].lower() == "x" and \
                            task["status"] == "needsAction":
                        # Todo has been completed, task was already there
                        completeTask(service, tasklist["id"], task, todo)
                    if todo.strip() != task["title"].strip() or \
                            (todo[:2].lower() == "x" and
                                todo[13:].strip() != task["title"].strip()):
                        # Todo was found, but mismatches title, so overwrite
                        updateTask(service, tasklist["id"], task, todo)
        if not found:
            addToTasks(service, todo)


def main(argv):
    if len(argv) < 2 or argv[0] == "usage":
        usage()
        sys.exit(2)

    if argv[1].lower() == "pull":
        print( "Pulling from Google Tasks" )
        pull()
    elif argv[1].lower() == "push" and len(argv) < 3:
        print( "Pushing to Google Tasks" )
        push("todo")
    elif argv[1].lower() == "push" and argv[2].lower() == "all":
        print( "Pushing all to Google Tasks" )
        push("todo")
        push("done")
    else:
        usage()


if __name__ == "__main__":
    main(sys.argv[:])
