import requests

URL = "http://localhost:9001"

# create a contact
post_resp = requests.post(
    URL + '/contacts',
    json={
        'name': 'Alice Adams',
        'phone': '919-555-6712',
        'address': '42 Wallaby Way, Sydney, NC'
    }
)
post_resp.raise_for_status()
print('created a contact:', post_resp.json())

first_contact_id = post_resp.json()['contact_id']

# update the contact
put_resp = requests.put(URL + '/contacts/' + first_contact_id, {'phone': '919-444-5601'})
put_resp.raise_for_status()
print('\nupdated a contact:', put_resp.json())

# add two more contacts
requests.post(
    URL + '/contacts',
    json={
        'name': 'Bob Billiham',
        'phone': '860-555-1409',
        'address': '3712 Not Real Lane, Bridgeport, CT'
    }
).raise_for_status()
requests.post(
    URL + '/contacts',
    json={
        'name': 'Cathy Caritgan',
        'phone': '512-555-6925',
        'address': '11 Stringigent Road, Albany, NY'
    }
).raise_for_status()
print('\added two more contacts')

# fetch contacts with 'billi' in the lowercased text
resp = requests.get(URL + '/contacts?name=billi')
resp.raise_for_status()
print('\ncontacts w/ name Bill:', resp.json())

# delete a contact
requests.delete(URL + '/contacts/' + first_contact_id).raise_for_status()
print('\ndeleted a contact')

# show all of the remaining contacts
resp = requests.get(URL + '/contacts')
resp.raise_for_status()
print('\nall contacts:', resp.json())
