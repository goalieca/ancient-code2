#include <pthread.h>
#include <fcntl.h>
#include <atomic>
#include <sys/mman.h>
#include <iostream>
#include <unistd.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <errno.h>

pthread_once_t init_lock = PTHREAD_ONCE_INIT;
int lock_fd = -1;
std::atomic<uint_fast64_t>* counter;

static void init()
{
    bool copy_zero = false;
    lock_fd = shm_open("db_lock", O_CREAT | O_RDWR, 0644);
    flock( lock_fd, LOCK_EX );
    {
        struct stat buf;
        int ret = fstat( lock_fd, &buf );
        std::cout<<"fstat() = "<<ret<<" errno "<<errno<<std::endl;
        std::cout<<"buf.st_size "<<buf.st_size<<std::endl;
        if (buf.st_size == 0)
        {
            std::cout<<"nulling "<<sizeof(std::atomic<uint_fast64_t>)<<" bytes\n";
            // nulls the bytes
            ftruncate(lock_fd, sizeof(std::atomic<uint_fast64_t>));
        }
        counter = reinterpret_cast<std::atomic<uint_fast64_t>*>(mmap( NULL, sizeof(std::atomic<uint_fast64_t>), PROT_READ | PROT_WRITE, MAP_SHARED, lock_fd, 0));
        std::cout<<"counter = "<<counter<<std::endl;
    }
    flock( lock_fd, LOCK_UN );
    std::cout<<"lock_fd "<<lock_fd<<std::endl;   
}

int main(int argc, char** argv)
{
    pthread_once(&init_lock, init);
    std::cout<<"file lock init\n";
    flock( lock_fd,  LOCK_EX );
    for (int i=0; i<11; i++)
    {
        uint_fast64_t count = counter->fetch_add(1);
        std::cout<<10-i<<":"<<count<<std::endl;
        if (i == 4)
            flock(lock_fd, LOCK_UN);
        sleep(1);
    }

    close(lock_fd);

    //by not unlinking, this preserves the count
    //shm_unlink( "db_lock" );
    return 0;
}
