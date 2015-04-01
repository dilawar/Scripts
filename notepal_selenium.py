from selenium import webdriver
import getpass

def main():
    browser = webdriver.Firefox()
    browser.get('https://doqcs.ncbs.res.in/notepal2015')
    #name = browser.find_element_by_id('edit-name')
    #name.send_keys("dilawars")
    #password = browser.find_element_by_id('edit-pass')
    #passw = getpass.getpass()
    #password.send_keys(passw)

    #submit = browser.find_element_by_id('edit-submit')
    #submit.submit()

if __name__ == '__main__':
    main()
