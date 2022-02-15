from sys import byteorder
import dpkt

def get_mqtt_frames(n_frames):
    frames = []
    with open("./source/mqtt_packets.pcap", 'rb') as captured_pcap:
        fpcap = dpkt.pcap.Reader(captured_pcap)
        for _,buf in fpcap:
            eth = dpkt.ethernet.Ethernet(buf)
            try:
                mqtt = eth.data[54:]
            except Exception as err:
                print(err)
                continue
            frames.append(mqtt)
            if n_frames == 0:
                break
            n_frames -= 1
    return frames

def get_mqtt_header(frame):
    #header = int.from_bytes(frame[0], byteorder="little")
    header = frame[0]
    return header

def get_mqtt_msg_len(frame):
    idx = 1
    msg_len = 0
    val_shift = 0
    bytes_read = 0
    while True:
        val = frame[idx]
        idx += 1
        if val <= 128:
            msg_len = msg_len + (val << val_shift)
            bytes_read += 1
            break
        val -= 128
        msg_len = msg_len + (val << val_shift)
        val_shift += 7
        bytes_read += 1
    return msg_len, bytes_read

def get_mqtt_topic_len(frame, index):
    a = frame[index]
    b = frame[index + 1]
    return b + (a<<8)

def main():
    frames = get_mqtt_frames(500)
    headers = []
    for f in frames:
        header = get_mqtt_header(f)
        if header != 50:
            continue
        msg_len, bytes_read = get_mqtt_msg_len(f)
        topic_length = get_mqtt_topic_len(f, bytes_read+1)
        topic = f[bytes_read+3:bytes_read+3+topic_length]
        headers.append([x for x in f[:bytes_read+3+topic_length]])
        

    with open(file="./source/mqtt_headers.txt", mode="w") as file:
        for h in headers:
            for el in h:
                file.write(str(el) + " ")
            file.write("\n")

if __name__ == "__main__":
    main()
    