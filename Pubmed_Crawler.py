
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 24 11:33:50 2018

@author: davidtzeng
"""
import requests
from lxml import html
import re
import urllib
import time
import numpy as np
from lxml import etree
from selenium import webdriver
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException
from collections import Counter
import matplotlib.pyplot as plt


class NcbiInfo(object):
    browser = webdriver.Chrome('./chromedriver')
    start_url = 'https://www.ncbi.nlm.nih.gov/pubmed/?term='
    wait = WebDriverWait(browser, 10)
    url = 'https://www.ncbi.nlm.nih.gov/pubmed/{}'
    header={
		'Host': 'www.ncbi.nlm.nih.gov',
		'Referer':start_url,
		'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:60.0) Gecko/20100101 Firefox/60.0',
	 }

    def main(self):
        self.get_url()
        time.sleep(1)
        self.is_page()
        flag = True
        if self.ispage == False:
           flag = False 
        else:
            self.click_yearandabstract()
            self.get_response()
        while True:
            if flag == False:
                break
            if self.ispage == False:
                print("t2")
                break
            self.get_info()
            self.next_page()
            if self.status == False:
                self.get_detail()
                #self.plot_curve()
                break
            else:
                self.nextpage.click()
                self.get_response()

    def __init__(self, keywordlist):
        self.temp = [urllib.parse.quote(i) for i in keywordlist]
        self.keyword_pre = '%2C'.join(self.temp)
        self.k1 = self.keyword_pre.split('%27')[1]
        self.k2 = self.keyword_pre.split('%27')[3]
        self.keyword = self.k1 + '+' + self.k2
        self.items = 0
        str(self.keyword).replace('%20',' ')
        self.title = ' AND '.join(self.keyword)
        self.url = NcbiInfo.start_url + self.keyword
        self.headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36'}
        self.file = open('output/' + self.keyword + '_information.txt', 'w')
        self.status = True
        self.yearlist = []
        self.pmidlist = []
        self.ispage = True

    def get_url(self):
        self.browser.get(self.url)
        
    def click_yearandabstract(self, ):
        #years = self.wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, '#_ds1 > li > ul > li:nth-child(1) > a')))
        #years.click()
        perpage = self.wait.until(EC.element_to_be_clickable((By.XPATH, '//ul[@class="inline_list left display_settings"]/li[3]/a/span[4]')))
        perpage.click()
        page_200 = self.wait.until(EC.element_to_be_clickable(
            (By.CSS_SELECTOR, '#display_settings_menu_ps > fieldset > ul > li:nth-child(6) > label')))
        page_200.click()

    def get_response(self):
        self.html = self.browser.page_source
        self.doc = etree.HTML(self.html)

    def is_page(self):
        self.html = self.browser.page_source
        self.doc = etree.HTML(self.html)
        time.sleep(1)
        self.ispage = self.doc.xpath('//dl[@class="rprtid"]/dd/text()')
        if len(self.ispage):
            self.ispage = True
        else:
            self.ispage = False
        
    def get_info(self):
        print(self.url)
        self.file.write(self.keyword + "\t")
        article_count_ = self.doc.xpath('//*[@id="maincontent"]/div/div[3]/div[1]/h3/text()')[0]
        if 'of' not in article_count_:
            self.items = article_count_.split(': ')[1]
            print(self.items)
        else:
            items2 = article_count_.split('of ')[0]
            self.items = items2.split('to')[1]
            print(self.items)
        self.art_timeanddoi = self.doc.xpath('//div[@class="rprt"]/div[2]/div/p[@class="details"]/text()')
        self.pmid = self.doc.xpath('//dl[@class="rprtid"]/dd/text()')
        for i in self.pmid:
            self.pmidlist.append(i)
        print(self.pmidlist)
        for i in self.art_timeanddoi:
            self.yearlist.append(i[2:6])
        for i in self.yearlist:
            if re.match('2', i):
                continue
            else:
                self.yearlist.remove(i)


    def next_page(self):
        try:
            self.nextpage = self.wait.until(
                EC.element_to_be_clickable((By.XPATH, '//*[@title="Next page of results"]')))
        except TimeoutException:
            self.status = False
    
    def get_detail(self):
        header={
                'Host': 'www.ncbi.nlm.nih.gov',
                'Referer':self.url,
                'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:60.0) Gecko/20100101 Firefox/60.0',
        }
        url = 'https://www.ncbi.nlm.nih.gov/pubmed/{}'
        res = requests.get(self.url, headers=header)
        #soup = BeautifulSoup(res.text,'html.parser')
        tree = html.fromstring(res.content)
        for pmid in self.pmidlist:
            each_url = url.format(pmid)		
            res = requests.get(each_url)
            tree = html.fromstring(res.content)
            print("\nPaper:")
            title = tree.xpath('//h1/text()')
            print(title[0],end='')
            self.file.write(title[0] + "\t")
            print("\nAuthor:")
            auther = tree.xpath('//div[@class="auths"]/a/text()')
            for auter in auther:	
                print(auter,end=",")
            print("\nAbstract:")
            abstract = tree.xpath('//div[@class=""]/p/text()')
            if abstract:
                print(abstract[0],end="")
                self.file.write(abstract[0] + "\t")
            else:
                continue
            print("\nPMID:")
            print(pmid)
            self.file.write(pmid + "\n")
            doi = tree.xpath('//dl[@class="rprtid"]/dd/a/text()')
            if doi:
                print("\nDOI:")
                print(doi[0])    
        self.file.close()
            
            
    def plot_curve(self):
        self.counter = Counter(self.yearlist)
        self.dic = dict(self.counter)
        self.keys = sorted(list(self.dic.keys()))
        self.curcount = 0
        self.y = []
        temp = [int(i) for i in self.keys]
        for i in range(min(temp), max(temp)+1):
            if str(i) in self.keys:
                self.curcount += self.dic[str(i)]
                self.y.append(self.curcount)
            else:
                self.y.append(self.curcount)
        plt.figure(figsize=(8, 5))
        plt.rcParams['font.sans-serif'] = ['SimHei']
        plt.rcParams['axes.unicode_minus'] = False
        plt.xlabel('Year')
        plt.ylabel('Paper Counts')
        #plt.title(self.title)
        plt.plot(np.arange(min(temp), max(temp)+1), np.array(self.y), 'r', marker='+', linewidth=2)
        plt.show()



if __name__ == '__main__':
    genus=[]
    with open('data/Bacteria_Genus.txt','r') as g:
        for line in g:
            genus.append(list(line.strip('\n').split('\t')))
        
    symptom=[]
    with open('data/Symptom.txt','r') as s:
        for line in s:
            symptom.append(list(line.strip('\n').split('\t')))
    for term1 in genus:
        print(term1)
        for term2 in symptom:
            print(term2)
            a = NcbiInfo([str(term1), str(term2)])
            a.main()
