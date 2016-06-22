"""
ihong @ 2016-June-22 /
9f office, sinopac
"""

from collections import Counter

import re
import string 
import pymssql
import pandas as pd 

server = 'dbm_public'
UID = 'xxxx'
PWD = 'xxxxxxxx'
DB = 'External'
conn = pymssql.connect(server,UID,PWD,DB)
cursor = conn.cursor()
sql = """
        select top 100 [發文日期],[作者],[標題],[內文],[IP位址] 
        from dbo.v_PTT信用卡版_銀行別
      """
cursor.execute(sql)
# cursor.fetchone() ## (datetime,u'title')

data = pd.DataFrame(cursor.fetchall(),columns= [u'發文日期',u'作者',u'標題',u'內文',u'IP位址']) 
data[u'IP位址'][0:5]
data[u'標題'][0:5]


## n-gram 
def ngrams(input_strs, length):
    for input_str in input_strs.split("\n"):
        for i in range(len(input_str)-length+1):
            yield input_str[i:i+length]


## clear Corpus....
def clearCorpus(s):
    # input s, return only chinese,eng,(ignore any punctuation,numbers)
    return ''.join(re.findall(ur'[\u4E00-\u9FD5]+',s))


corpus_db = data[u'內文']
doc1 = corpus_db[0]
doc2 = corpus_db[1]
print clearCorpus(doc1)


print " ".join(ngrams(u"這是一段文字\n這是另一段文字",2) )
print " ".join(ngrams(clearCorpus(doc1),2))

### Count , unigram/bigram/trigram------
uni_freq = Counter(ngrams(clearCorpus(doc1), 1))
print "".join([" %s : %s \n" % (w[0], w[1]) for w in uni_freq.most_common(40)])

bi_freq = Counter(ngrams(clearCorpus(doc1),2))
print "".join([" %s : %s \n" %(w[0],w[1]) for w in bi_freq.most_common(40)])

tri_freq = Counter(ngrams(clearCorpus(doc1),3))
print "".join(["%s : %s\n" %(w[0],w[1]) for w in tri_freq.most_common(40)])

##
corpus =''
for e in corpus_db:
    corpus =  corpus + '\n' + clearCorpus(e)

print corpus
uni_freq = Counter(ngrams(corpus,1))
bi_freq = Counter(ngrams(corpus,2))
tri_freq = Counter(ngrams(corpus,3))

print "".join([" %s : %s \n" %(w[0],w[1]) for w in bi_freq.most_common(40)])
print "".join(["%s : %s\n" %(w[0],w[1]) for w in tri_freq.most_common(40)])

#### language model #############################

## uni_probability

def uni_prob(w0):
    ## counting the probability of w0, for given corpus 
    m_uni_freq = uni_freq.get(w0, 0)
    m_total = len(corpus)
    return m_uni_freq / float(m_total)
# eg... p(w3,w2,w1) = p(w3)p(w2)p(w1)
uni_prob(u'信')*uni_prob(u'用')*uni_prob(u'卡')
uni_prob(u'用')*uni_prob(u'信')*uni_prob(u'卡')
uni_prob(u'信')*uni_prob(u'卡')*uni_prob(u'用')

## bi_probability 

def bi_prob(w1, w0):
    m_bi_freq = bi_freq.get(w0+ w1, 0)
    m_uni_freq = uni_freq.get(w0, 1)
    return m_bi_freq / float(m_uni_freq)
# eg 信用卡 /用信卡/卡用信
uni_prob(u'信')*bi_prob(u'用',u'信')*bi_prob(u'卡',u'用') # p(信用卡) = p(信)p(用|信)p(卡|用) 
uni_prob(u'用')*bi_prob(u'信',u'用')*bi_prob(u'卡',u'信')# p(用信卡) = p(用)p(信|用)p(卡|信)
uni_prob(u'卡')*bi_prob(u'用',u'卡')*bi_prob(u'信',u'用')# p(卡用信) = p(卡)*p(用|卡)*(信|用)


## tri_probability
def tri_prob(w2,w0,w1):
    m_tri_freq = tri_freq.get(w0+w1+w2,0)
    m_bi_freq = bi_freq.get(w0+w1,1)
    return m_tri_freq/float(m_bi_freq)

## eg 信用卡申請 vs 申信請卡用

uni_prob(u'信')*bi_prob(u'用',u'信')*tri_prob(u'卡',u'信',u'用')*tri_prob(u'申',u'用',u'卡')\
*tri_prob(u'請',u'卡',u'申') #p(信)p(用|信)p(卡|信用)p(申|用卡)p(請|卡申)
uni_prob(u'申')*bi_prob(u'信',u'申')*tri_prob(u'請',u'申',u'信')*tri_prob(u'卡',u'信',u'請')\
*tri_prob(u'用',u'請',u'卡') #p(申)p(信|申)p(請|申信)p(卡|信請)p(用|請卡)


#### auto_complete ...
def auto_completion(w1):
    g3s = filter(lambda x: x[0] == w1, tri_freq.iterkeys())
    result_all = []
    for g3 in g3s:
        (w1, w2, w3) = g3
        p = bi_prob(w2, w1) * tri_prob(w3, w1, w2)
        if p > 0:
            result_all.append((p, w2 + w3))
    result = sorted(result_all, key=lambda x: x[0], reverse=True)[:10]
    for i, w in enumerate(result):
        print i + 1, w[0], w[1]

auto_completion(u'信')
auto_completion(u'國')


### 
