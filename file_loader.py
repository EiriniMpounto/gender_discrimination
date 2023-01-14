import argparse
 

class DataLoader(object):
    def __init__(self):
        arg_parser = argparse.ArgumentParser()
        arg_parser.add_argument("-w", "--workingdirectory", help="Working Directory")
        arg_parser.add_argument(
            "-c", "--ceoofficialdataset", help="Ceo official dataset"
        )

        args = arg_parser.parse_args()
        self.working_directory = args.workingdirectory
        self.ceo_official_data = args.ceoofficialdataset
        # concatenate the file with the directory
        
        self.ceo_official_data = f"{self.working_directory}/{self.ceo_official_data}"
        