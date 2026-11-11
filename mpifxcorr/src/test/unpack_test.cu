/* 
Generate some nonsense data and feed it to the new general unpack decoder, and compare
to the *numerous* decoders in mark5access
*/

#include <mark5access.h>
#include <iostream>
#include <bitset>
#include "gpudecode.cuh"

#define FAKE_LENGTH 1048576

int no_next(struct mark5_stream *ms) {
    return 0;
}

int always_valid(const struct mark5_stream *ms) {
    return 0;
}

void reset_mk5stream(struct mark5_stream *ms) {
    ms->readposition = 0;
}

float **run_decoder(struct mark5_stream *ms, int nsamp, int nchan, int nbit, int decimation) {
    std::cout << "Decoding with... samples = " << nsamp << "\tchannels = " << nchan << 
    "\tbits = " << nbit << "\tdecimation = " << decimation << std::endl;

    // Make space for the unpacked data
    float **unpacked;
    cudaMallocManaged(&unpacked, nchan * sizeof(float*));
    for (int i=0; i<nchan; i++){
        cudaMallocManaged(&unpacked[i], nsamp * sizeof(float));

        // Make sure everything is set to zero
        for (int j = 0; j < nsamp; j++) {
            unpacked[i][j] = 0.0;
        }
    }

    std::cout << "Running decoder..." << std::endl;
    // Run the decoder...
    std::cout << "End = " << ms->blankzoneendvalid[0] << std::endl;
    int read = ms->decode(ms, nsamp, unpacked);
    std::cout << "Read : " << read << "/" << nsamp << std::endl;

    return unpacked;
}


int decode_wrapper(struct mark5_stream *ms, int nsamp, float **unpacked) {
    int *goodsamples;
    cudaMallocManaged(&goodsamples, sizeof(int));
    mk5_decode_general_gpu<<<1, 1>>>(*ms, nsamp, unpacked, goodsamples);
    cudaDeviceSynchronize();
    return *goodsamples;
}


int test_decoder(struct mark5_stream *ms, int nsamp, int nchan, int nbit, int decimation) {
    int framesperperiod = 1; 
    int alignmentseconds = 0;
	int databytesperpacket = 1024;
    int frameheadersize = 32;
    int usecomplex = 0;

    // Get the required cpu decoder
    std::cout.setstate(std::ios_base::failbit);
    mark5_format_generic *fake_format_generic = new_mark5_format_generalized_vdif(framesperperiod, alignmentseconds, 
        nchan, nbit, decimation, databytesperpacket, frameheadersize, usecomplex);
    std::cout.clear();
    if (fake_format_generic == 0) {
        return 2;
    }
    ms->decode = fake_format_generic->decode;

    // for (int i = 0; i < nsamp; i++) {
    //     std::cout << std::bitset<8>(ms->payload[i]) << std::endl;
    // }

    std::cout << "CPU : ";
    float **cpu_decoded = run_decoder(ms, nsamp, nchan, nbit, decimation);
    // for (int i = 0; i < nsamp; i++) {
    //     for (int chan = 0; chan < nchan; chan++) {
    //         std::cout << cpu_decoded[chan][i] << " ";
    //     }
    //     std::cout << std::endl;
    // }

    // Reset the stream to the beginning
    reset_mk5stream(ms);

    // Do the GPU decoder now
    std::cout << "GPU : ";
    ms->decode = decode_wrapper;
    float **gpu_decoded = run_decoder(ms, nsamp, nchan, nbit, decimation);
    // for (int i = 0; i < nsamp; i++) {
    //     for (int chan = 0; chan < nchan; chan++) {
    //         std::cout << gpu_decoded[chan][i] << " ";
    //     }
    //     std::cout << std::endl;
    // }

    reset_mk5stream(ms);

    // Compare the results for a given number of samples and channels
    for (int chan = 0; chan < nchan; chan++) {
        for (int i = 0; i < nsamp; i++) {
            if (abs(cpu_decoded[chan][i] - gpu_decoded[chan][i]) > 0.0001 ) {
                std::cout << "Test failed. " << std::endl;
                std::cout << "#Channels : " << nchan << std::endl;
                std::cout << "#Bits : " << nbit << std::endl;
                std::cout << "Decimation : " << decimation << std::endl;
                std::cout << "CPU = " << cpu_decoded[chan][i] << "\tGPU = " << gpu_decoded[chan][i] << "\tsample = " << i <<  std::endl;
                return 0;
            }
        }
    }

    // Free the decoded data
    for (int i=0; i<nchan; i++){
        cudaFree(cpu_decoded[i]);
        cudaFree(gpu_decoded[i]);
    }
    cudaFree(cpu_decoded);
    cudaFree(gpu_decoded);

    return 1;
}


int main() {
    // Make some fake data - can literally be anything; just need some zeros and ones to work with
    //unsigned char data[FAKE_LENGTH];
    unsigned char *data;
    cudaMallocManaged(&data, FAKE_LENGTH * sizeof(char));
    for (int i = 0; i < FAKE_LENGTH; i++) {
        data[i] = (unsigned char)rand();
    }

    // Loop through all valid configs
    int success = 0;
    int fail = 0;

    for (int chan = 1; chan <= 64; chan++) {
        for (int bit = 1; bit <= 16; bit *= 2) {
            for (int d = 1; d <= 1; d*=2) {
                int Mbps = 1;
                int nchan = chan;
                int nbit = bit;
                int decimation = d;
                int nsamp = 32;

                // Create a stream object with just enough structure to pass through the decoders
                mark5_stream fake_stream;
                fake_stream.payload = data;
                fake_stream.readposition = 0;
                fake_stream.blankzonestartvalid[0] = 0;
                fake_stream.blankzoneendvalid[0] = INT32_MAX;
                fake_stream.next = *no_next;
                fake_stream.validate = *always_valid;
                fake_stream.nchan = nchan;
                fake_stream.nbit = nbit;
                fake_stream.decimation = decimation;
                fake_stream.databytes = INT32_MAX;

                // Run a test
                int result = test_decoder(&fake_stream, nsamp, nchan, nbit, decimation);
                if (result == 1) {
                    std::cout << "Test successful!" << std::endl;
                    success += 1;
                } else if (result == 0) {
                    std::cout << "Test failed :( " << std::endl;
                    fail += 1;
                    return 1;
                }
            }
        }
    }

    std::cout << std::endl << "Summary:" << std::endl;
    std::cout << "Successful decodes: " << success << std::endl;
    std::cout << "Failed decodes: " << fail << std::endl;

    // TODO: Time comparison (just on cpu versions to see if my algorithm is slower)

}

