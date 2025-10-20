import json
import subprocess


if __name__ == "__main__":
    output = subprocess.run(
        'cmake -E capabilities',
        capture_output=True, shell=True, check=True,
    ).stdout.decode('utf-8')
    data = json.loads(output)
    generators = [
        'Ninja' if x['name'] == 'Ninja Multi-Config' else x['name']
        for x in data['generators']
    ]
    print(";".join(generators))
