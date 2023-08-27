#!/usr/bin/env python
# Simple script converting json nets to the binary representation used by Carp
import sys
import json
import struct

FEATURES = 768
HIDDEN = 768
QA = 255
QB = 64
QAB = QA * QB
PARAM_SIZE = 2 # param size in bytes

NET_FILE = "../../../bins/net.bin"

def write_bytes(array):
    with open(NET_FILE, 'ab') as file:
        for num in array:
            file.write(struct.pack('<h', num))
        
        # Pad the array so we get 64B alignment
        overhead = ((len(array) * 2) % 64)
        if overhead != 0:
            padding = 64 - overhead
            file.write(struct.pack('<' + str(padding) + 'x'))
        

def convert_weight(json_weight, stride, length, q, transpose):
    weights = [0 for _ in range(length)]

    for i, row in enumerate(json_weight):
        for j, weight in enumerate(row):
            if transpose:
                index = j * stride + i
            else:
                index = i * stride + j
            
            weights[index] = int(weight * q)

    return weights

def convert_bias(json_bias, q):
    biases = []

    for bias in json_bias:
        value = int(bias * q)
        biases.append(value)
    
    return biases

# Check for correct number of command line arguments
if len(sys.argv) != 2:
    print("Usage: python convert_json.py <json_file>")
    sys.exit(1)

json_file = sys.argv[1]
with open(json_file, 'r') as file:
    data = json.load(file)

feature_weights = convert_weight(data["ft.weight"], HIDDEN, HIDDEN * FEATURES, QA, True)
feature_biases = convert_bias(data["ft.bias"], QA)
output_weights = convert_weight(data["out.weight"], HIDDEN * 2, HIDDEN * 2, QB, False)
output_biases = convert_bias(data["out.bias"], QAB)  

# Clear the old net and write the new data (ordering is important!)
open(NET_FILE, 'w').close()
write_bytes(feature_weights)
write_bytes(feature_biases)
write_bytes(output_weights)
write_bytes(output_biases)
