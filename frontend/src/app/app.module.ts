import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { RouterModule } from '@angular/router';

import { AppComponent } from './app.component';
import { TaskListComponent } from './task-list/task-list.component';
import { NewTaskComponent } from './new-task/new-task.component';

@NgModule({
    declarations: [
        AppComponent,
        TaskListComponent,
        NewTaskComponent
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        RouterModule.forRoot([
            {
                path: 'task-list',
                component: TaskListComponent
            },
            {
                path: 'new-task',
                component: NewTaskComponent
            },
            {
                path: '',
                redirectTo: '/task-list',
                pathMatch: 'full'
            }
 
        ])
    ],
    providers: [],
    bootstrap: [AppComponent]
})
export class AppModule { }
