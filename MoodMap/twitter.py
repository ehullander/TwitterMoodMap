import tweepy
import sys
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

#Variables that contains the user credentials to access Twitter API 
access_token =  "893920530-48B8jawsRQ6vtQwXygSnYizF7s4xXpu0fcIcwQni"
access_token_secret = "1jGpacXsU94KVibBXbIAsDIfXFLg1Rf6WELNOfTuFYBM4"
consumer_key = "IXbJQnvb801OIel741LqPEL4q"
consumer_secret = "dmqUVhpnLh4XcXDMT8na0Ql61KGaDRvLXAyIiHBadu1EsCHCj1"
qty = int(sys.argv[1])

#This is a basic listener that just prints received tweets to stdout.
class StdOutListener(StreamListener):

    tweetCounter=0

    def on_data(self, data):
        while True:
            StdOutListener.tweetCounter=StdOutListener.tweetCounter+1
            try:
                print data   
                if  StdOutListener.tweetCounter < qty:
                    return True
                else:
                    return False
            except:
                continue
        

    def on_error(self, status):
        print status


if __name__ == '__main__':

    #This handles Twitter authetification and the connection to Twitter Streaming API
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)

#This line filter Twitter Streams to capture data by the keywords: 'python', 'javascript', 'ruby'
    germany = [5.0770049095, 47.2982950435, 15.0403900146, 54.9039819757]
    USA=[-125.0011, 24.9493, -66.9326, 49.5904]
    #stream.filter(track=['happy', 'sad'])
    stream.filter(locations=USA)
