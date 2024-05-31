#!/usr/bin/env python

'''
Module      : count_guides
Description : Count CRISPR guide sequences
Copyright   : (c) MAREK CMERO, 2023
License     : TBD
Maintainer  : Marek Cmero
Portability : POSIX
Takes a fastq file and tab-delimited text file containing CRISPR library guide
sequences and counts the number of times each sequence appears in the fastq.
'''

import os
import sys
import gzip
import logging
import pandas as pd
import pyfastx as fx
import re
import edlib
from io import StringIO
from datetime import datetime
from argparse import ArgumentParser

LIBRARY_FILE_COLNAMES = ['library', 'sequence', 'gene', 'other']
GUIDE_LEN_WARNING = 'WARNING: Insert length param does not match library'


def init_log(filename):
    '''
    Initialise logging
    '''
    logging.basicConfig(filename=filename,
                        level=logging.DEBUG,
                        filemode='w',
                        format='%(asctime)s %(levelname)s - %(message)s',
                        datefmt="%Y-%m-%dT%H:%M:%S%z")


def parse_args():
    '''Parse arguments'''
    description = '''
        Takes a fastq file and tab-delimited text file or
        fasta file containing CRISPR library guide sequences
        '''
    parser = ArgumentParser(description=description)
    parser.add_argument('-r1',
                        '--read1',
                        metavar='R1',
                        type=str,
                        help='Read 1 file in fastq format.')
    parser.add_argument('-l',
                        '--library',
                        metavar='LIB',
                        type=str,
                        required=True,
                        help='''
                        Fasta file containing guide sequences, or
                        comma-separated gzipped file containing
                        CRISPR library guide sequences.''')
    parser.add_argument('-i',
                        '--guide-len',
                        metavar='LEN',
                        default=20,
                        type=int,
                        help='Length of guide sequence.')
    parser.add_argument('-p',
                        '--primer',
                        metavar='PRIMER',
                        type=str,
                        default="",
                        help='''Primer sequence to trim from the 5' end of the
                        guide sequence. Default is no primer (guide is at read
                        start.''')
    parser.add_argument('-m',
                        '--primer-mismatches',
                        metavar='M',
                        type=int,
                        default=1,
                        help='''Number of mismatches allowed when matching
                        primer sequences. Default is 1.''')
    parser.add_argument('-g',
                        '--genomics-barcodes',
                        action='store_true',
                        help='''Whether to expect Genomics barcode format
                        for file name (e.g., Fwd_01-Rev_01_R1.fastq).
                        Default is False.''')
    parser.add_argument('-I',
                        '--infer-primer',
                        action='store_true',
                        help='Infer primer sequence.')
    parser.add_argument('-n',
                        '--num-infer',
                        metavar='N',
                        type=int,
                        default=20,
                        help='Number of reads to use for primer inference.')
    parser.add_argument('-P',
                        '--primer-len',
                        metavar='PL',
                        type=int,
                        default=30,
                        help='Length of primer sequence for inference.')

    return parser.parse_args()


def infer_primer(read1, num_reads, primer_len):
    '''
    Infer primer sequence from first num_reads reads
    '''
    reads_processed = 0
    primer_dict = {}
    primer = ""

    for r1 in read1:
        reads_processed += 1
        if reads_processed > num_reads:
            break

        try:
            name, seq, qual = r1
        except Exception as e:
            logging.info('Error reading read(s) %s (%s)' % (r1.name, type(e)))
            continue

        tmp_primer = seq[:primer_len]
        if tmp_primer in primer_dict:
            primer_dict[tmp_primer] += 1
        else:
            primer_dict[tmp_primer] = 1

    if len(primer_dict) == 0:
        logging.error('Failed to infer primer sequence')
    else:
        primer = max(primer_dict, key=primer_dict.get)

    return primer


def read_crispr_library(library_file, guide_len):
    if library_file.endswith(('csv', 'csv.gz')):
        crispr_library = pd.read_csv(library_file, sep=',', header=None)
        if len(crispr_library.columns) != 4:
            raise ValueError('CRISPR library file must have 4 columns')

        # reload with header if necessary
        if crispr_library.iloc[0, 0].lower() == LIBRARY_FILE_COLNAMES[0]:
            crispr_library = pd.read_csv(library_file, sep=',', header=0)
            if not all(crispr_library.columns == LIBRARY_FILE_COLNAMES):
                raise ValueError('Invalid column names in library file')
        else:
            crispr_library.columns = LIBRARY_FILE_COLNAMES

        # check that guide length is correct
        cur_guide_len = crispr_library.sequence.map(len).unique()[0]
        if cur_guide_len != guide_len:
            logging.info(GUIDE_LEN_WARNING)
            logging.info('Guide length set to %d' % cur_guide_len)
            guide_len = cur_guide_len
    elif library_file.endswith(('fasta', 'fa')):
        crispr_library = pd.DataFrame()
        fasta = fx.Fasta(library_file, build_index=False, full_name=True)
        for name, seq in fasta:
            if len(seq) != guide_len:
                logging.info(GUIDE_LEN_WARNING)
                logging.info('Guide length set to %d' % cur_guide_len)
                guide_len = len(seq)
            record_to_add = pd.DataFrame(
                    {'library': os.path.basename(library_file).split('.')[0],
                     'sequence': seq,
                     'gene': name.split(' ')[0],
                     'other': ':'.join(name.split(' ')[1:])},
                    index=[0])
            crispr_library = pd.concat([crispr_library, record_to_add],
                                       ignore_index=True)
    else:
        raise ValueError('Invalid library file: expected csv.gz or fasta.')

    return crispr_library, guide_len


def count_amplicons(read1, guide_len, primer="", primer_mismatches=1):
    reads_processed = 0
    failed_reads = 0
    amplicon_dict = {}
    primer_len = len(primer)

    for r1 in read1:
        reads_processed += 1
        if reads_processed % 10000 == 0:
            logging.info('Processed %d reads' % reads_processed)

        try:
            name, seq, qual = r1
        except Exception as e:
            logging.info('Error reading read(s) %s (%s)' % (r1.name, type(e)))
            failed_reads += 1
            continue

        primer_start = 0
        if primer_len > 0:
            # check if primer is present
            primer_result = edlib.align(primer, seq, mode="HW",
                                        task="path", k=primer_mismatches)
            edit_distance = primer_result['editDistance']
            locations = primer_result['locations']
            if edit_distance > primer_mismatches or len(locations) == 0:
                logging.info('Failed to match primer in read %s' % name)
                failed_reads += 1
                continue
            primer_start = primer_result['locations'][0][0]

        primer_offset = primer_start + primer_len
        guide = seq[primer_offset:(guide_len+primer_offset)]
        if guide in amplicon_dict:
            amplicon_dict[guide] += 1
        else:
            amplicon_dict[guide] = 1

    logging.info('Processed %d reads' % reads_processed)
    logging.info('Failed to process %d reads' % failed_reads)
    return amplicon_dict


def match_counts(basename, counts, crispr_library,
                 output_colnames, genomics_barcodes=False):
    '''
    Match counts to CRISPR library guide sequences
    '''
    results = pd.DataFrame.from_dict(counts, orient='index', columns=['count'])
    results['guide'] = results.index
    results = pd.merge(results, crispr_library,
                       left_on='guide', right_on='sequence')

    if genomics_barcodes:
        # expected file name example: Fwd_01-Rev_01_R1.fastq
        # add index information (obtained from read file name)
        tmp = basename.split('-')
        fwd_idx_name = tmp[0]
        rev_idx_name = '_'.join(tmp[1].split('_')[:2])
        results['FwdBarcode'] = fwd_idx_name
        results['RevBarcode'] = rev_idx_name
    else:
        results['sample'] = basename

    # sort and reorder columns
    results = results.sort_values(by='count', ascending=False)
    results = results[output_colnames]

    logging.info('Assigned %d reads.' % results['count'].sum())
    return results


def is_file_empty(file):
    '''
    Check if file is empty
    '''
    if file.endswith('.gz'):
        with gzip.open(file, 'rt') as f:
            char = f.read(1)
            return not char

    return os.stat(file).st_size == 0


def main():
    '''
    Main function
    '''
    args = parse_args()

    # initialise logging
    basename = os.path.splitext(os.path.basename(args.read1))[0]
    basename = os.path.splitext(basename)[0]  # split again for .fastq.gz files
    logfile = f'{basename}_' + datetime.now().strftime('%H-%M-%d-%m-%Y.log')
    init_log(logfile)

    # write arguments to log
    logging.info('Arguments:')
    for arg in vars(args):
        logging.info('%s: %s' % (arg, getattr(args, arg)))

    # if genomics barcodes are specified, check that the input format is right
    output_colnames = ['sample', 'guide', 'library', 'gene', 'other', 'count']
    if args.genomics_barcodes:
        regex = r'Fwd_[0-9]*\-Rev_[0-9]*_R[1,2]'
        if not re.search(regex, basename):
            logging.error('''
                          File names do not match expected Genomic
                          sbarcode format (e.g., Fwd_01-Rev_01_R1).''')
            sys.exit(1)
        output_colnames = ['FwdBarcode', 'RevBarcode', 'guide',
                           'library', 'gene', 'other', 'count']

    # handle empty input fastq files
    if is_file_empty(args.read1):
        logging.info('Read 1 file is empty. Writing empty output file.')
        print('\t'.join(output_colnames), file=sys.stdout)
        return

    # infer primer sequence if infer_primer > 0 is specified
    if args.infer_primer:
        logging.info('Infering primer sequence')
        read1 = fx.Fastq(args.read1, build_index=False)
        primer = infer_primer(read1, args.num_infer, args.primer_len)
        logging.info('Inferred primer sequence: %s' % primer)
    else:
        primer = args.primer

    # read in CRISPR library
    guide_len = args.guide_len
    crispr_library, guide_len = read_crispr_library(args.library, guide_len)

    # read in fastq
    read1 = fx.Fastq(args.read1, build_index=False)
    counts = count_amplicons(read1, guide_len,
                             primer, args.primer_mismatches)

    # write counts to file
    counts = match_counts(basename, counts, crispr_library,
                          output_colnames, args.genomics_barcodes)

    # write to stdout
    output = StringIO()
    counts.to_csv(output, sep='\t', index=False)
    output.seek(0)
    print(output.read(), file=sys.stdout)


if __name__ == '__main__':
    main()
