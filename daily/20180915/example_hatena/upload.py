#!/home/pi/.pyenv/shims/python
# hatenablog_post_v7.py
#coding=utf-8
import os
import sys
import random
import requests
from base64 import b64encode
from datetime import datetime
from hashlib import sha1
from pathlib import Path
from chardet.universaldetector import UniversalDetector
import re
from time import sleep
import shutil
import glob

now = datetime.now()
dtime = str(now.year) + """-""" + str(now.month) + """-""" + str(now.day) + """T""" + str(
    now.hour
) + """:""" + str(now.minute) + """:""" + str(now.second)
print(dtime)

# setting -----------------------------------------------------------
username = 'はてなブログに登録している名前（ここではshut9）'
api_key = '********APIキーをここに書く*******'
blogname = 'ブログの名前を書く（ここではsaibaimen.hatenadiary.jp）'
draft = 'yes'  # yes or no    下書きとして投稿する場合はyes。本投稿はno。


# WSSE
def wsse(username, api_key):
    created = datetime.now().isoformat() + "Z"
    b_nonce = sha1(str(random.random()).encode()).digest()
    b_digest = sha1(b_nonce + created.encode() + api_key.encode()).digest()
    c = 'UsernameToken Username="{0}", PasswordDigest="{1}", Nonce="{2}", Created="{3}"'
    return c.format(username, b64encode(b_digest).decode(), b64encode(b_nonce).decode(), created)


# hatena blog
def create_data_blog(title, body, fotoname):
    if fotoname == None:
        text = body
    else:
        text = body + """                                                                                                                                                                                   
                                                                                                                                                                                                            
[f:id:shut9:{0}p:plain]                                                                                                                                                                                     
        """.format(fotoname)

    template = """<?xml version="1.0" encoding="utf-8"?>                                                                                                                                                    
    <entry xmlns="http://www.w3.org/2005/Atom" xmlns:app="http://www.w3.org/2007/app">                                                                                                                      
    <title>{0}</title>                                                                                                                                                                                      
    <author><name>{1}</name></author>                                                                                                                                                                       
    <content type="text/x-markdown">                                                                                                                                                                        
{2}                                                                                                                                                                                                         
    </content>                                                                                                                                                                                              
    <updated>{3}</updated>                                                                                                                                                                                  
    <category term="" />                                                                                                                                                                                    
    <app:control>
    <app:draft>{4}</app:draft>                                                                                                                                                                              
    </app:control>                                                                                                                                                                                          
    </entry>
    """

    data = template.format(title, username, text, dtime, draft).encode()
    return data


def parse_text(file, charset):
    with open(file, encoding=charset) as f:
        obj = f.readlines()
        body = ""
        for i, line in enumerate(obj):
            body = body + line
    return body


def check_encoding(file):
    detector = UniversalDetector()
    with open(file, mode='rb') as f:
        for binary in f:
            detector.feed(binary)
            if detector.done:
                break
    detector.close()
    charset = detector.result['encoding']
    return charset


def post_hatena(data, headers):
    url = 'http://blog.hatena.ne.jp/{0}/{1}/atom/entry'.format(username, blogname)
    r = requests.post(url, data=data, headers=headers)

    if r.status_code != 201:
        sys.stderr.write(
            'Error!\n' + 'status_code: ' + str(r.status_code) + '\n' + 'message: ' + r.text
        )


# hatena foto life
def create_data_foto(filename):
    files = Path(filename).read_bytes()
    root, ext = os.path.splitext(filename)
    ext = ext[1:]

    uploadData = b64encode(files)

    template = """                                                                                                                                                                                            
    <entry xmlns="http://purl.org/atom/ns#">                                                                                                                                                                
    <title>{0}</title>                                                                                                                                                                                      
    <content mode="base64" type="image/{1}">{2}</content>                                                                                                                                                   
    </entry>                                                                                                                                                                                                
    """

    return template.format(filename, ext, uploadData.decode())


def upload_foto(data, headers):
    url = 'http://f.hatena.ne.jp/atom/post/'
    r = requests.post(url, data=data, headers=headers)

    if r.status_code != 201:
        sys.stderr.write(
            'Error!\n' + 'status_code: ' + str(r.status_code) + '\n' + 'message: ' + r.text
        )


def foto_info(headers):
    url = 'http://f.hatena.ne.jp/atom/feed/'
    r = requests.post(url, headers=headers)

    return re.search('[0-9]{14}', r.text).group()


# main -----------------------------------------------------------
def main():

    # define WSSE header
    headers = {'X-WSSE': wsse(username, api_key)}

    # upload photo to HatenaFotoLife
    filelist = glob.glob('/home/pi/log_condition/log_pic/*')
    filename = '/home/pi/log_condition/log_pic/{0:%Y%m%d}12.png'.format(now)
    print(filename)
    fotoflag = 0
    if filename in filelist:
        print('Photo uploading...')
        data_foto = create_data_foto(filename)
        upload_foto(data_foto, headers)
        fotoflag = 1
        sleep(10)
    else:
        print('Photo data is nothing')
        fotoflag = 0

    # confirm infomation of uploaded pyhoto
    if fotoflag == 1:
        print('---- Uploaded foto info ----')
        fotoname = foto_info(headers)
        print(fotoname)
    else:
        fotoname = None

    # post blog to HatenaBlog
    filelist_log = glob.glob('/home/pi/log_condition/log_TH/*')
    filename_log = '/home/pi/log_condition/log_TH/{0:%Y%m%d}.txt'.format(now)
    print(filename_log)
    if filename_log in filelist_log:
        print('blog uploading...')
        charset = check_encoding(filename_log)
        title = '今日の観察'
        body = parse_text(filename_log, charset)
        data_blog = create_data_blog(title, body, fotoname)
    else:
        print("log file is nothing")
        return 0

    post_hatena(data_blog, headers)
    print('Done')


if __name__ == '__main__':
    main()
