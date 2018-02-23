import pandas as pd
import numpy as np

class CryptoPricePredictor(object):
    """docstring for CryptoPricePredictor."""
    def __init__(self):
        super(CryptoPricePredictor, self).__init__()



if __name__ == '__main__':
    CryptoPricePredictor()

def read_data(uri):
    """Given a URI, return processed data"""
    df = pd.read_csv(uri)
    # do additional data processing here


