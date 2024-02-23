# Import smtplib for the actual sending function
import smtplib
import sys

# Import the email modules we'll need
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email import Encoders
from email.MIMEText import MIMEText

# Open a plain text file for reading.  For this example, assume that
# the text file contains only ASCII characters.
#fp = open("message.txt", 'rb')
# Create a text/plain message

msg = MIMEMultipart()
# me == the sender's email address
# you == the recipient's email address
msg['Subject'] = sys.argv[3]
msg['From'] = sys.argv[1]
msg['To'] = sys.argv[2]

#part = MIMEBase('application', "octet-stream")
#part.set_payload(open("message.txt", "rb").read())
#Encoders.encode_base64(part)

#part.add_header('Content-Disposition', 'attachment; filename="text.txt"')
#msg.attach(part)

body = MIMEMultipart('alternative')
body.attach(MIMEText(open(sys.argv[4], "rb").read()))
#body.attach(MIMEText(html, 'html')) 

msg.attach(body)

# Send the message via our own SMTP server, but don't include the
# envelope header.
#s = smtplib.SMTP('bluefish1')
s = smtplib.SMTP(sys.argv[5])
s.sendmail(sys.argv[1], [sys.argv[2]], msg.as_string())

s.quit()
