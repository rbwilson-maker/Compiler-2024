package edu.cmu.cs411.java.server;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Vector;
import java.util.stream.Collectors;

/**
 * ABOUT : 
 * Sandboxes resources in jobs. Can have shared resources over multiple jobs
 * Resources under a jobclassloader has a different class space from the rest 
 * of the server, and each child under each job will have a different class 
 * space from its sibling.
 * This allows us to have many __main_c0__ classes under this server without
 * having to use a different VM
 * 
 * EDIT 1 : 
 * Fixed potential concurrency issue when dealing from the command line
 *  Problem : 
 *      1. client makes a loader under 0
 *      2. client sends a job under loader 0 
 * Race condition : we send the job before the loader is actually made
 * Fix : we wait when we send a job to a loader that does not exist
 * 
 * Potential race condition : 
 *  When we rebind the resources or add resources before we exit.
 *  Resolve : whatever comes first -- the external client should wait till 
 *    the first call has exited. 
 * 
 */
public class JobClassLoader {
    private final List<Object> LOCKS = new Vector<>(4);
    private final List<ClassLoaderDelegate> childLoaders = new Vector<>(4);
    
    private static final class ClassLoaderDelegate extends URLClassLoader {
        ClassLoaderDelegate() {
            super(new URL[0], null);
        }

        void addResources(List<URL> resources) {
            resources.forEach(this::addURL);
        }

        ClassLoader newChild(List<URL> additionalResources){
            return new URLClassLoader(additionalResources.toArray(new URL[0]), this);
        }
    }
    private ClassLoader newJob(int loaderNum) {
        expandTo(loaderNum);
        
        //wait for the loader to become available
        while(childLoaders.get(loaderNum) == null){
            try{
                final Object LOCK = LOCKS.get(loaderNum);
                synchronized(LOCK){
                    LOCK.wait();
                }
            } catch(InterruptedException e){}
        }
        ClassLoader re = childLoaders.get(loaderNum);
        assert re != null;
        return re;
    }
    
    private ClassLoader newJob(int loaderNum, List<URL> additionalResources){
        return ((ClassLoaderDelegate)newJob(loaderNum))
                .newChild(Objects.requireNonNull(additionalResources));
    }
    
    private List<Class<?>> loadClasses(
            final ClassLoader loader,
            List<String> classesToLoad
        ) {
        return Objects.requireNonNull(classesToLoad)
            .parallelStream()
            .map(s -> {
                try{return loader.loadClass(s);}
                catch(ClassNotFoundException e){return null;}
            })
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

/* BEGIN methods you should care about */
    public Class<?> loadClass(
            int loaderNum, 
            List<URL> additionalResources,
            String classToLoad) throws ClassNotFoundException{
        ClassLoader context = newJob(loaderNum, additionalResources);
        return context.loadClass(classToLoad);
    }
    
    public List<Class<?>> loadClasses(
            int loaderNum, 
            List<URL> additionalResources,
            List<String> classesToLoad) {
        return loadClasses(
            newJob(loaderNum, additionalResources),
            classesToLoad);
    }
    
    public List<Class<?>> loadClasses(
            int loaderNum,
            List<String> classesToLoad) {
        return loadClasses(
            newJob(loaderNum),
            classesToLoad);
    }
    
    private void expandTo(int idx){
        while(childLoaders.size() <= idx)
            childLoaders.add(null);
        while(LOCKS.size() <= idx)
            LOCKS.add(new Object());
    }
    
    private void newLoader0(int loaderNum) throws IOException{
        expandTo(loaderNum);
        ClassLoaderDelegate l = childLoaders.set(loaderNum, new ClassLoaderDelegate());
        if(l!=null) l.close();
    }
    
    public void newLoader(int loaderNum) throws IOException {
        newLoader0(loaderNum);
        final Object LOCK = LOCKS.get(loaderNum);
        synchronized(LOCK){
            LOCK.notifyAll();
        }
    }
    
    public void newLoaderWithResources(int loaderNum, List<URL> resources) throws IOException{
        newLoader0(loaderNum);
        addResources(loaderNum, resources);
        final Object LOCK = LOCKS.get(loaderNum);
        synchronized(LOCK){
            LOCK.notifyAll();
        }
    }
    
    public void deleteLoader(int loaderNum) throws IOException{
        Objects.requireNonNull(childLoaders.set(loaderNum, null))
            .close();
    }
    
    public void addResources(int loaderNum, List<URL> resources) {
        Objects.requireNonNull(
                childLoaders.get(loaderNum), 
                "Create a resource loader you dingus")
            .addResources(Objects.requireNonNull(resources));
    }
    
    public void destroy(){
        childLoaders.
            parallelStream()
            .forEach(l -> {
                try{l.close();}
                catch(IOException e){}
            });
        childLoaders.clear();
        LOCKS.clear();
    }
}