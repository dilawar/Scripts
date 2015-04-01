from selenium import webdriver

def main():
    #browser = webdriver.Firefox()
    browser = webdriver.PhantomJS()
    browser.get('https://doqcs.ncbs.res.in/notepal2015')
    name = browser.find_element_by_id('edit-name')
    name.send_keys("dilawars")
    password = browser.find_element_by_id('edit-pass')
    password.send_keys("password")

    submit = browser.find_element_by_id('edit-submit')
    submit.submit()

if __name__ == '__main__':
    main()
