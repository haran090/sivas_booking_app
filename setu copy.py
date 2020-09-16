import jwt
import datetime
import uuid
import requests
import json

def createLink():
	# Prod
	scheme_id = "0d61c678-9b54-4637-8e0d-a905a8489b50"
	secret    = "9bc4a564-a718-4790-80b3-24337f1c3f64"
	instance = "446521760280479286"
	url = "http://prod.setu.co/api/payment-links"

	#Test
	#scheme_id = "4f32fd01-796c-4963-972b-0067cc845fa6"
	#secret    = "247e87ce-558d-4473-83ba-f6b692404114"
	#instance = "421106595821258021"
	#url = "https://sandbox.setu.co/api/payment-links"

	payload   = {
	    "aud" : scheme_id,
	    "iat" : datetime.datetime.utcnow(),
	    "jti" : str(uuid.uuid1())
	}

	token = jwt.encode(payload, secret, algorithm="HS256")

	payload = "{\"amount\":{\"value\": 50000,\"currencyCode\": \"INR\"},\"amountExactness\": \"EXACT\",\"billerBillID\": \"video-bill-" + str(uuid.uuid1()) + "\", \"name\": \"Bill for video appointment\"}"

	headers = {
	    "Authorization": "Bearer " + token.decode("utf-8"),
	    "X-Setu-Product-Instance-ID": instance,
	    "Content-Type": "application/json"
	}

	response = json.loads(requests.request("POST", url, data=payload, headers=headers).text)

	output = {}
	output['url'] = response['data']['paymentLink']['shortURL']
	output['upiID'] = response['data']['paymentLink']['upiID']
	output['billID'] = response['data']['platformBillID']

	return output



def sendSMS(number, link):
	url = "https://www.fast2sms.com/dev/bulk"

	querystring = {
		"authorization":"614QcHUn9mdBE2wLy5zOftYopKs3kWDAhjau0MqFZ8bVl7CRerwzBheor7Dk9CycAftMTpSql614EFaJ",
		"sender_id":"SIV",
		"message":"35938",
		"language":"english",
		"route":"qt",
		"numbers":number,
		"variables_values":" " + link + " .",
		"variables":"{FF}"
	}

	payload = ""
	headers = {
	    'cache-control': "no-cache"
	    }

	response = requests.request("GET", url, data=payload, headers=headers, params=querystring)

	print(response.text) 
